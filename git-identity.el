;;; git-identity.el --- Identity management for (ma)git -*- lexical-binding: t -*-

;; Copyright (C) 2019,2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1") (dash "2.10") (hydra "0.14") (f "0.20"))
;; Keywords: git vc convenience
;; URL: https://github.com/akirak/git-identity.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This Emacs package lets you manage local Git identities, i.e.
;; user.name and user.email options in .git/config, inside
;; Emacs.  It can be useful if you satisfy all of the following
;; conditions:

;; - You have multiple Git identities on the same machine(s).
;; 
;; - You use Emacs.
;; 
;; - You almost always use magit for Git operations on your
;;   machine(s).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'f)
(require 'dash)
(require 'hydra)

(defgroup git-identity nil
  "Identity management for Git."
  :group 'vc)

;;;; Custom vars

;;;###autoload
(defcustom git-identity-git-executable "git"
  "Executable file of Git."
  :group 'git-identity
  :type 'string)

;;;###autoload
(defcustom git-identity-default-username
  (when (and (stringp user-full-name)
             (not (string-empty-p user-full-name)))
    user-full-name)
  "Default full name of the user set in Git repositories."
  :group 'git-identity
  :type 'string)

;;;###autoload
(defcustom git-identity-list nil
  "List of plists of Git identities."
  :group 'git-identity
  :type '(alist :tag "Identity setting"
                :key-type (string :tag "E-mail address (user.email)")
                :value-type (plist :tag "Options"
                                   :options
                                   (((const :tag "Full name" :name)
                                     string)
                                    ((const :tag "Host names" :domains)
                                     (repeat string))
                                    ((const :tag "Directories" :dirs)
                                     (repeat string))))))

(defcustom git-identity-verify t
  "When non-nil, check if the identity is consistent.

This check is run if the repository doesn't have a local
identity.
If the repository is expected to have a certain
identity according to the domain name or the local directory (see
`git-identity-list'), and the identity is different from the
global setting, `git-identity-magit-mode' asks if you want to
add a local identity setting to the repository.
This ensures that your identity policies defined in
`git-identity-list' are applied properly when you have a global
identity setting."
  :group 'git-identity
  :type 'boolean)

;;;; Faces
(defface git-identity-bold-face
  '((t :inherit bold))
  "Face for bold text.")

(defface git-identity-success-face
  '((t :inherit success))
  "Face for identity information consistent with the expected.")

(defface git-identity-warning-face
  '((t :underline (:style wave :color "Red1") :inherit warning))
  "Face for identity information inconsistent with the expected.")

(defface git-identity-mismatch-face
  '((t :underline (:style wave :color "Red1") :inherit default))
  "Face for identity information inconsistent with the expected.")

(defface git-identity-local-identity-face
  '((t :bold t :inherit success))
  "Face for bold text.")

(defface git-identity-global-identity-face
  '((t :bold t :inherit default))
  "Face for bold text.")

;;;; Macros
(defmacro git-identity--with-extra-message (string &rest progn)
  (declare (indent 1))
  `(let ((buf (when ,string (generate-new-buffer "*git identity info*"))))
     (save-window-excursion
       (unwind-protect
           (progn
             (when buf
               (when (and (bound-and-true-p lv-wnd)
                          (window-live-p lv-wnd))
                 (delete-window lv-wnd))
               (with-current-buffer buf
                 (setq-local header-line-format nil)
                 (setq-local mode-line-format nil)
                 (setq cursor-type nil)
                 (insert (string-trim ,string)))
               (display-buffer-at-bottom buf nil)
               (fit-window-to-buffer (get-buffer-window buf)))
             ,@progn)
         (when buf
           (progn
             (let ((bufw (get-buffer-window buf)))
               (when bufw
                 (quit-window nil bufw))
               (kill-buffer buf))))))))
(defcustom git-identity-repository-read-function nil
  "Function used to read a repository URL in the package.

This should be a function that returns a string.

If this value is nil, the built-in function is used."
  :group 'git-identity
  :type '(or function nil))

;;;; Identity operations
(defun git-identity--username (identity)
  "Extract the user name in IDENTITY or return the default."
  (or (plist-get (cdr identity) :name)
      (git-identity--default-username)))

(defun git-identity--email (identity)
  "Extract the email in IDENTITY."
  (car identity))

(defun git-identity--default-username ()
  "Retrieve the default user name."
  (or git-identity-default-username
      (customize-set-variable
       'git-identity-default-username
       (read-string "Enter your full name used as the default: "
                    nil nil
                    (when (and (stringp user-full-name)
                               (not (string-empty-p user-full-name)))
                      user-full-name)))))

(defun git-identity--domains (identity)
  (plist-get (cdr identity) :domains))

(defun git-identity--dirs (identity)
  (plist-get (cdr identity) :dirs))

;;;; Guessing identity for the current repository

(cl-defun git-identity--guess-identity (&key verbose)
  "Pick an identity which seems suitable for the current repo."
  (let ((url (or (git-identity--git-config-get "remote.origin.pushurl")
                 (git-identity--git-config-get "remote.origin.url"))))
    (-find (lambda (ent)
             ;; Which should take precedence? Domain or directory?
             (let ((plist (cdr ent)))
               (or (when url
                     (let ((domain (git-identity--host-in-git-url url)))
                       (when (-contains? (plist-get plist :domains) domain)
                         (when verbose
                           (message "Chosen an identity based on domain %s in url \"%s\""
                                    domain url))
                         t)))
                   (let ((ancestor (git-identity--inside-dirs-p default-directory
                                                                (plist-get plist :dirs))))
                     (when ancestor
                       (when verbose
                         (message "Chosen an identity based on an ancestor directory %s"
                                  ancestor))
                       t)))))
           (cl-defun git-identity--guess-identity (&optional (log t))
             "Pick an identity which seems suitable for the current repo.

When LOG is nil, suppress the log message shown when a matching
identity is found."
             (let ((url (git-identity--repository-remote-url)))
               (-find (lambda (ent)
                        ;; Which should take precedence? Domain or directory?
                        (or (when url
                              (git-identity--identity-match-url-p
                               url ent
                               :log log))
                            (git-identity--identity-match-directory
                             default-directory ent
                             :log log)))
                      git-identity-list)))

(cl-defun git-identity--guess-identity-for-url (url &optional (log t))
  "Pick an identity from the URL.

URL is a Git repository URL.

When LOG is nil, suppress the log message shown when a matching
identity is found."
  (-find (lambda (ent)
           (git-identity--identity-match-url-p url ent :log log))
         git-identity-list))

(defun git-identity--repository-remote-url ()
  "Get a URL of the origin repository if any."
  (or (git-identity--git-config-get "remote.origin.pushurl")
      (git-identity--git-config-get "remote.origin.url")))

(cl-defun git-identity--identity-match-url-p (url entry &key log)
  "Return non-nil if an identity entry matches the url.

URL is the location of the Git repository which should be a
supported Git URL, and ENTRY is an identity entry.

If LOG is t, display a message in the minibuffer when the entry
matches."
  (let ((domain (git-identity--host-in-git-url url)))
    (when (-contains? (git-identity--domains entry) domain)
      (when log
        (message "Chosen an identity based on domain %s in url \"%s\""
                 domain url))
      t)))

(cl-defun git-identity--identity-match-directory (dir entry &key log)
  "Return non-nil if an identity entry matches the directory.

DIR is the path to the Git repository on a local file system, and
ENTRY is an identity entry.

If LOG is t, display a message in the minibuffer when the entry
matches."
  (let* ((ancestor (git-identity--inside-dirs-p
                    dir (git-identity--dirs entry))))
    (when ancestor
      (when log
        (message "Chosen an identity based on an ancestor directory %s"
                 ancestor))
      t)))

(defun git-identity--host-in-git-url (url)
  "Extract the host from URL of a Git repository."
  (cond
   ((string-match (rx bol "https://" (group (+ (not (any "/:"))))) url)

    (match-string 1 url))
   ((string-match (rx bol (?  (+ (not (any "@:"))) "@")
                      (group (+ (not (any ":"))))) url)
    (match-string 1 url))))

(defun git-identity--inside-dirs-p (target maybe-ancestors)
  "Return non-nil if TARGET is a descendant of any of MAYBE-ANCESTORS."
  (let ((abs-target (expand-file-name target)))
    (--some (f-ancestor-of-p (expand-file-name it) abs-target)
            maybe-ancestors)))

;;;; Querying and setting an identity in a repository

;;;###autoload
(defun git-identity-complete (prompt)
  "Display PROMPT and select an identity from `git-identity-list'."
  (let ((input (completing-read prompt git-identity-list
                                nil nil nil nil
                                (car
                                 (git-identity--guess-identity :verbose t)))))
    (or (assoc input git-identity-list)
        (if (git-identity--validate-mail-address input)
            (let* ((name (read-string "Name: "))
                   (newent (list input :name name)))
              (customize-set-variable git-identity-list
                                      (cons newent git-identity-list)))
          (user-error "Not a valid mail address: %s" input)))))

(defun git-identity--validate-mail-address (_input)
  "Return non-nil if _INPUT is a valid e-mail address."
  ;; TODO: Really validate the input
  t)

;;;###autoload
(defun git-identity-set-identity (&optional prompt)
  "Set the identity for the repository at the working directory.

This function lets the user choose an identity for the current
repository using `git-identity-complete' function and sets the
user name and the email address in the local configuration of the
Git repository.

Optionally, you can set PROMPT for the identity.
If it is omitted, the default prompt is used."
  (interactive)
  (let ((root (git-identity--find-repo)))
    (unless root
      (user-error "Not in a Git repository"))
    (let* ((default-directory root)
           (identity (git-identity-complete
                      (or prompt
                          (format "Select an identity in %s: " root)))))
      (git-identity--set-identity identity))))

(defun git-identity--has-local-identity-p ()
  "Return non-nil If the current repository has an identity."
  (and (git-identity--git-config-get "user.name" "--local")
       (git-identity--git-config-get "user.email" "--local")))

(defun git-identity--find-repo (&optional startdir)
  "Find a Git working directory which is STARTDIR or its ancestor."
  (let ((startdir (or startdir default-directory)))
    (if (file-directory-p ".git")
        startdir
      (locate-dominating-file startdir ".git"))))

(cl-defun git-identity--set-identity (identity &key no-confirm prompt)
  "Set the identity in the current repo to IDENTITY."
  (declare (indent 2))
  (when (or no-confirm
            (git-identity--with-extra-message
                (git-identity--format-repository-identity identity)
              (yes-or-no-p
               (or prompt "Are you sure you want to set these settings in .git/config?"))))
    (git-identity--git-config-set-no-confirm
     "user.name" (git-identity--username identity)
     "user.email" (git-identity--email identity))))

(defun git-identity--pad-string (str width)
  "Left-pad STR to WIDTH.

The string can be nil.  In that case, it is considered as an empty
string, so the entire width will be filled up with whitespaces."
  (let* ((str (or str ""))
         (len (length str)))
    (if (< len width)
        (concat str (make-string (- width len) ?\ ))
      str)))

(defun git-identity--format-repository-identity (&optional expected-identity description)
  "Return the string to summarize the identity information of the repository.

EXPECTED-IDENTITY is an identity object to represent the expected
identity of the repository.

DESCRIPTION is a string to describe the current situation of the
identity setting."
  (let* ((repo (abbreviate-file-name (git-identity--find-repo)))
         (origin (or (git-identity--git-config-get "remote.origin.pushurl" "--local")
                     (git-identity--git-config-get "remote.origin.url" "--local")))
         (local-name (git-identity--git-config-get "user.name" "--local"))
         (local-email (git-identity--git-config-get "user.email" "--local"))
         (global-name (git-identity--git-config-get "user.name" "--global"))
         (global-email (git-identity--git-config-get "user.email" "--global"))
         (expected-name (when expected-identity (git-identity--username expected-identity)))
         (expected-email (when expected-identity (git-identity--email expected-identity)))
         (current-name (or local-name global-name))
         (current-email (or local-email global-email))
         (width1 (when expected-identity
                   (max (length current-name) (length current-email))))
         (width2 (when expected-identity
                   (max (length expected-name) (length expected-email))))
         (has-local local-email))
    (string-join `(,(concat "Repository " repo)
                   ,(concat "Origin: " (or origin "Not set"))
                   ,(if expected-identity
                        (concat
                         " +" (make-string (+ 14 width1 width2) ?-) "+\n"
                         (mapconcat (pcase-lambda (`(,header ,current ,expected))
                                      (string-join (list ""
                                                         (git-identity--pad-string header 6)
                                                         (git-identity--pad-string current width1)
                                                         (git-identity--pad-string expected width2)
                                                         "")
                                                   " | "))
                                    `((nil ,(if has-local
                                                "Current (local) "
                                              "Current (global)")
                                           "Expected")
                                      ("Name"
                                       ,(propertize (or current-name "N/A")
                                                    'face
                                                    (if (equal current-name expected-name)
                                                        'git-identity-success-face
                                                      'git-identity-warning-face))
                                       ,(if (equal current-name expected-name)
                                            expected-name
                                          (propertize expected-name
                                                      'face 'git-identity-mismatch-face)))
                                      ("E-mail"
                                       ,(propertize (or current-email "N/A")
                                                    'face
                                                    (if (equal current-email expected-email)
                                                        'git-identity-success-face
                                                      'git-identity-warning-face))
                                       ,(if (equal current-email expected-email)
                                            expected-email
                                          (propertize expected-email
                                                      'face 'git-identity-mismatch-face))))
                                    "\n")
                         "\n +" (make-string (+ 14 width1 width2) ?-) "+")
                      (if (and current-name current-email)
                          (format "Current: %s <%s>" current-name current-email)
                        "Current: Missing"))
                   ,(if (and current-name current-email)
                        (format "The current identity is based on the %s gitconfig."
                                (if has-local
                                    (propertize "local" 'face 'git-identity-local-identity-face)
                                  (propertize "global" 'face 'git-identity-global-identity-face)))
                      "Cannot find the current identity."))
                 "\n")))

;;;; Hydra

(defhydra git-identity-hydra ()
  "
Git identity in %s(git-identity--find-repo)
=======================================================
User name: %(git-identity--git-config-get \"user.name\")
E-mail: %s(git-identity--git-config-get \"user.email\")
-------------------------------------------------------
"
  ("s" (progn
         (git-identity-set-identity)
         (hydra-keyboard-quit))
   "Set an identity")
  ("C" (customize-variable 'git-identity-list)
   "Configure your identities"))

;;;###autoload (autoload 'git-identity-info "git-identity")
(defalias 'git-identity-info 'git-identity-hydra/body
  "Display the identity information of the current repository.")

(defun git-identity--block-if-not-in-repo (orig &rest args)
  "Prevent running ORIG function with ARGS if not in a Git repo."
  (if (git-identity--find-repo)
      (apply orig args)
    (user-error "Not inside a Git repo")))

(advice-add #'git-identity-info :around #'git-identity--block-if-not-in-repo)

;;;; Mode definition
(defun git-identity-ensure ()
  "Ensure that the current repository has an identity."
  (let ((local-email (git-identity--git-config-get "user.email" "--local"))
        (local-name (git-identity--git-config-get "user.name" "--local"))
        (global-email (git-identity--git-config-get "user.email" "--global"))
        (global-name (git-identity--git-config-get "user.name" "--global"))
        (expected-identity (git-identity--guess-identity)))
    (cond
     ;; There is an expected identity, and it matches the identity
     ;; effective in the repository.
     ((and expected-identity
           (or local-email global-email)
           (string-equal (or local-email global-email)
                         (git-identity--email expected-identity))
           (string-equal (or local-name global-name)
                         (git-identity--username expected-identity))))
     ;; No local identity is configured yet, but there is an expected identity.
     ((not (git-identity--has-local-identity-p))
      (or (and expected-identity
               (git-identity--set-identity expected-identity
                   :prompt "This repository lacks an expected local identity. Set the identity above?"))
          (git-identity-set-identity "No proper identity is not set. Select one: ")))
     ;; There is no local setting, and the global setting is contradictory
     ;; with the expectation. Ask if you want to apply the local setting.
     ((and git-identity-verify
           (not local-email)
           (not (equal (git-identity--email expected-identity)
                       global-email)))
      (git-identity--set-identity expected-identity
          :prompt "The global identity violates the expected local identity of this repository. Set the local identity above? ")))))

;;;; Cloning Git repositories
;;;###autoload
(defun git-identity-clone (url)
  "Clone URL to a destination depending on its URL."
  (interactive (list (git-identity--read-repository-url)))
  ;; Find a matching identity
  (let* ((identity (or (git-identity--guess-identity-for-url url)
                       (git-identity-complete
                        (format "Choose an identity for %s: " url))))
         (dirs (git-identity--dirs identity))
         (parent (completing-read (format "Select a cloning destination for %s: "
                                          url)
                                  dirs))
         (dest (read-directory-name (format "Enter a non-existent directory path for %s: "
                                            url)
                                    parent
                                    nil nil
                                    ;; TODO: Get the default name from the URL
                                    nil)))
    ;; TODO: Set the identity in the local repository.
    ;; I'll probably have to send a PR to magit package to implement this feature.
    ;;
    ;; TODO: Allow setting options (or allow choosing a clone command).
    (magit-clone-regular url dest nil)))

(defun git-identity--read-repository-url ()
  "Read a Git repository URL."
  (if (functionp git-identity-repository-read-function)
      (funcall git-identity-repository-read-function)
    (read-string "Repository URL: ")))

;;;; Git utilities
(defun git-identity--git-config-set-no-confirm (&rest pairs)
  "Set a PAIRS of Git options without user confirmation.

When successful, this function returns PAIRS."
  (cl-loop for (key value . _) on pairs by #'cddr
           do (git-identity--run-git "config" "--local" "--add" key value))
  pairs)

(defun git-identity--run-git (&rest args)
  "Run Git with ARGS."
  (apply #'call-process git-identity-git-executable nil nil nil args))

(defun git-identity--git-config-get (key &optional scope)
  "Get the value of a Git option.

KEY is the name of the option, and optional SCOPE is a string passed
as an argument to Git command to specify the scope, which is either
\"--global\" or \"--local\"."
  (with-temp-buffer
    (when (= 0 (apply #'call-process git-identity-git-executable nil t nil
                      (delq nil `("config" "--get" ,scope ,key))))
      (string-trim-right (buffer-string)))))

(provide 'git-identity)
;;; git-identity.el ends here
