;;; git-identity-github.el --- Integration with GitHub -*- lexical-binding: t -*-

(require 'ghub)
(declare-function 'url-encode-url "url-util")

;;;; Retrieve information from the server

(defun repom-github--list-user-repos (&optional update)
  "Return a list of user repositories.

If UPDATE is non-nil, first clear the cache."
  (repom--with-cache-variable repom-github-user-repos-cache
                              update
                              (ghub-request "GET" "/user/repos"
                                            `((visibility . "all")
                                              (sort . ,(symbol-name repom-github-user-repos-sort)))
                                            :unpaginate t
                                            :auth 'repom)))

(defun repom-github--list-starred-repos (&optional update)
  "Return a list of repositories starred by the user.

If UPDATE is non-nil, first clear the cache."
  (repom--with-cache-variable repom-github-starred-repos-cache
                              update
                              (ghub-request "GET" "/user/starred" nil
                                            :unpaginate t
                                            :auth 'repom)))

(defun repom-github--search-repos (query)
  "Search repositories matching QUERY."
  ;; TODO: Allow specify the sorting method
  (ghub-request "GET" "/search/repositories"
                `((q . ,query))
                :auth 'repom))

(provide 'git-identity-github)
;;; git-identity-github.el ends here
