;;; stash-mode.el -- Minor mode to interact with STASH (more slimmed down version of org-stash??)
;; 
;; Filename: stash-mode.el
;; Description: Minor mode for STASH that allows some very basic functionality
;; Author: Eric Hansen
;; Maintainer: Eric Hansen
;; Copyright (C) 2016 Eric Hansen, all rights reserved.
;; Created: April 18, 2016
;; Version: 0.0.1
;; URL: 
;; Keywords: Stash, minor mode
;; Compatibility: 23.2+
;; 
;; Features that might be required by this library:
;;
;;   json
;;   subr-x
;;   request
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; Allows various functionality such as creating pull requests (which is
;; arguably more of a Stash thing but hey).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 0.0.1 - initial alpha release
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Variables:
(defvar stash-mode-hook nil)

; String of the username used to log into Stash
(defvar stash-username nil)

; Password string used to log in
(defvar stash-password nil)

; List of strings you want to add for reviewers
(defvar stash-reviewers '())

; The URL of Stash so we can API this thing
(defvar stash-url nil)

; Keymap used for stash-mode
(defvar stash-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-s p") 'stash-pull-request)
    (define-key map (kbd "C-s i") 'stash-init)
    map)
  "Keymap for Stash mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Autoloads:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(add-to-list 'auto-mode-alist '("*\\.*\\'" . composer-mode))

;;;###autoload
(define-minor-mode stash-mode
  "composer.json mode"
  :lighter " STASH"
  :keymap stash-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Required libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'subr-x)
(require 'json)
(require 'request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Private functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exec (cmd)
  (string-trim-right (shell-command-to-string cmd)))

(defun git-exec (cmd)
  (exec (format "git %s" cmd)))

(defun match-regex (regex text)
  (string-match regex text))

;; Parse a Git URL to get the project and repository
(defun parse-git-url (url)
  ;; In case SSL is used here lets be a little friendly here.  This assumes match 1 is the project and match 2 is the repository
  (if (or (match-regex "http.://.*/\\([^/].*\\)/\\([^\.].*\\).git" url) (match-regex ".*@.*:\\([^/].*\\)/\\([^\.].*\\).git" url))
      (list (match-string 1 url) (match-string 2 url))))

;; Get the current branch we are on in Git
(defun git-current-branch ()
  (git-exec "branch | grep '*' | awk '{print $2}'"))

;; Create a URL that is pleasant to use
(defun create-stsah-url (project repo)
  (format "%s/rest/api/1.0/projects/%s/repos/%s/pull-requests" stash-url project repo))

(defun read-file (filename)
  (with-temp-buffer (insert-file-contents filename) (buffer-string)))

(defun build-reviewers-list (reviewers-list)
  (require 'json)
  (setq namehash (make-hash-table :test 'equal))
  (setq reviewers (make-vector (length reviewers-list) nil))
  (setq index 0)

  (dolist (reviewer reviewers-list)
    (puthash "user" (make-hash-table :test 'equal) namehash)
    (puthash "name" reviewer (gethash "user" namehash))
    (aset reviewers index namehash)
    (setq index (+ 1 index)))

  (json-encode reviewers))

(defun git-commits ()
  (git-exec "log --branches --not --remotes --oneline --format=%B"))

(defun stash-pr-create-request ()
  (format "{
    \"title\": \"<title>\",
    \"description\": \"<description>\",
    \"state\": \"OPEN\",
    \"open\": true,
    \"closed\": false,
    \"fromRef\": {
        \"id\": \"refs/heads/<source>\",
        \"repository\": {
            \"slug\": \"<repository>\",
            \"name\": null,
            \"project\": {
                \"key\": \"<project>\"
            }
        }
    },
    \"toRef\": {
        \"id\": \"refs/heads/<destination>\",
        \"repository\": {
            \"slug\": \"<repository>\",
            \"name\": null,
            \"project\": {
                \"key\": \"<project>\"
            }
        }
    },
    \"locked\": false,
    \"reviewers\": <reviewers>
}"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Public functions:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun stash-init ()
  "Calls git init on the current directory."
  (interactive)
  (unless (file-exists-p ".git")
    (git-exec "init")))

(defun stash-pull-request (destination-branch)
  "Creates a pull request and displays the link for any further use afterwards."
  (interactive "sDestination: ")
  (unless (and stash-username stash-password stash-url)
    (user-error "Username, password and URL for Stash must be set."))

  (let (
	(dir (pwd))
	(destpath (git-exec "config --get remote.origin.url"))
	(request (stash-pr-create-request))
	(current-branch (git-current-branch))
	(auth-header (format "Basic %s" (base64-encode-string (format "%s:%s" stash-username stash-password) t)))
	)
    (require 'request)
    (setq gitparts (parse-git-url destpath))
    (setq request (replace-regexp-in-string "<title>" current-branch request))
    (setq request (replace-regexp-in-string "<source>" current-branch request))
    (setq request (replace-regexp-in-string "<destination>" destination-branch request))
    (setq request (replace-regexp-in-string "<project>" (nth 0 gitparts) request))
    (setq request (replace-regexp-in-string "<repository>" (nth 1 gitparts) request))
    (setq request (replace-regexp-in-string "<reviewers>" (build-reviewers-list stash-reviewers) request))
    (setq request (replace-regexp-in-string "<description>" (git-commits) request))
    
    (message "%s" request)
;    (request
;     (create-stash-url (nth 0 gitparts) (nth 1 gitparts))
;     :type "POST"
;     :data request
;     :parser 'json-read
;     :headers '(("Content-Type" . "application/json")
;		("Content-Length" . (length request))
;		("Authorization" . auth-header))
;     :success (function* (lambda (&key data &allow-other-keys)
;			   (let ((url (assoc-default 'url (elt (assoc-default 'link data) 0))))
;			     (message "PR link: %s" url))
;			 )))
    ))

(provide 'stash-mode)
