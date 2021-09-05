;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'git-identity)

;; Based on the specification of git urls in the man page of git-push (1) for Git 2.28.0.

(defconst git-identity-test-identity-list
  '(("akira.komamura@gmail.com"
     :name "Akira Komamura"
     :domains ("github.com")
     :exclude-organizations ("my-company-org")
     :dirs ("~/work/github.com/"))
    ("akira.komamura@mycompany.com"
     :name "My name in the local language"
     :organizations ("my-company-org")
     :domains ("github.com" "mycompany.com")
     :dirs ("~/work/mycompany.com/"))))

(describe "Identity operations"
  (describe "git-identity-username"
    (it "Retrieves the user name of an identity"
      (let ((git-identity-list git-identity-test-identity-list))
        (expect (git-identity-username (car git-identity-list))
                :to-equal "Akira Komamura"))))
  (describe "git-identity-email"
    (it "Retrieve the email address of an identity"
      (let ((git-identity-list git-identity-test-identity-list))
        (expect (git-identity-email (nth 1 git-identity-list))
                :to-equal "akira.komamura@mycompany.com")))))

(describe "git-identity--host-in-git-url"

  (it "matches an SSH URL"
    (expect (git-identity--host-in-git-url "ssh://github.com/path/to/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "ssh://github.com:22/path/to/repo.git/")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "ssh://john.blah@github.com/path/to/repo.git/")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "ssh://user-13@github.com:22/path/to/repo.git/")
            :to-equal "github.com"))

  (it "matches an Git URL"
    (expect (git-identity--host-in-git-url "git://github.com/owner/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "git://github.com:22/owner/repo.git/")
            :to-equal "github.com"))

  (it "matches an HTTPS URL"
    (expect (git-identity--host-in-git-url "https://github.com/owner/repo")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "https://github.com/owner/repo/")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "https://github.com:22/owner/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "https://github.com:22/owner/repo.git/")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "https://hg.sr.ht/~geyaeb/haskell-pdftotext")
            :to-equal "hg.sr.ht"))

  (it "matches URLs of git-remote-hg"
    (expect (git-identity--host-in-git-url "hg::https://hg.sr.ht/~geyaeb/haskell-pdftotext")
            :to-equal "hg.sr.ht"))

  (it "matches an FTP URL"
    (expect (git-identity--host-in-git-url "ftp://github.com/owner/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "ftps://github.com/owner/repo.git/")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "ftp://github.com:22/owner/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "ftps://github.com:22/owner/repo.git/")
            :to-equal "github.com"))

  (it "matches an SCP-like syntax"
    (expect (git-identity--host-in-git-url "git@github.com:owner/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "github.com:owner/repo.git")
            :to-equal "github.com")
    (expect (git-identity--host-in-git-url "git@gist.github.com:1234123412341234.git")
            :to-equal "gist.github.com")
    (expect (git-identity--host-in-git-url "xxx@yyy.host.com:/owner/repo.git")
            :to-equal "yyy.host.com")))

(describe "git-identity--dir-in-git-url"

  (it "matches an SSH URL"
    (expect (git-identity--dir-in-git-url "ssh://github.com/path/to/repo.git")
            :to-equal "path/to")
    (expect (git-identity--dir-in-git-url "ssh://github.com:22/path/to/repo.git/")
            :to-equal "path/to")
    (expect (git-identity--dir-in-git-url "ssh://john.blah@github.com/path/to/repo.git/")
            :to-equal "path/to")
    (expect (git-identity--dir-in-git-url "ssh://user-13@github.com:22/path/to/repo.git/")
            :to-equal "path/to"))

  (it "matches an Git URL"
    (expect (git-identity--dir-in-git-url "git://github.com/owner/repo.git")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "git://github.com:22/owner/repo.git/")
            :to-equal "owner"))

  (it "matches an HTTPS URL"
    (expect (git-identity--dir-in-git-url "https://github.com/owner/repo")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "https://github.com/owner/repo/")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "https://github.com:22/owner/repo.git")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "https://github.com:22/owner/repo.git/")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "https://hg.sr.ht/~geyaeb/haskell-pdftotext")
            :to-equal "~geyaeb"))

  (it "matches URLs of git-remote-hg"
    (expect (git-identity--dir-in-git-url "hg::https://hg.sr.ht/~geyaeb/haskell-pdftotext")
            :to-equal "~geyaeb"))

  (it "matches an FTP URL"
    (expect (git-identity--dir-in-git-url "ftp://github.com/owner/repo.git")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "ftps://github.com/owner/repo.git/")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "ftp://github.com:22/owner/repo.git")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "ftps://github.com:22/owner/repo.git/")
            :to-equal "owner"))

  (it "matches an SCP-like syntax"
    (expect (git-identity--dir-in-git-url "git@github.com:owner/repo.git")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "github.com:owner/repo.git")
            :to-equal "owner")
    (expect (git-identity--dir-in-git-url "xxx@yyy.host.com:/owner/repo.git")
            :to-equal "owner")))

(provide 'git-identity-test)
