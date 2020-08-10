;;; git-identity-magit.el --- Integrate git-identity with magit -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
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

;; This library defines `git-identity-magit-mode', which integrates
;; git-identity with `magit-commit'.

;;; Code:

(autoload 'git-identity-ensure "git-identity")
(declare-function magit-commit "ext:magit-commit")

;;;###autoload
(define-minor-mode git-identity-magit-mode
  "Global minor mode for running Git identity checks in Magit.

This mode enables the following features:

- Add a hook to `magit-commit' to ensure that you have a
  global/local identity configured in the repository.
"
  :global t
  (cond
   ;; Activate the mode
   (git-identity-magit-mode
    (advice-add #'magit-commit :before #'git-identity-ensure))
   ;; Deactivate the mode
   (t
    (advice-remove #'magit-commit #'git-identity-ensure))))

(provide 'git-identity-magit)
;;; git-identity-magit.el ends here
