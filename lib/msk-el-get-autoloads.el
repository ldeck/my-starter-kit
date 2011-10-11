;;; msk-el-get-autoloads.el --- lib for my-starter-kit.el
;;
;; Copyright (c) 2011 Lachlan Deck and contributors
;;
;; Author: Lachlan Deck <lachlan.deck@gmail.com>
;; URL: http://github.com/ldeck/my-starter-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This lib file loads the user defined el-get sources.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; el-get and associated autoloaded packages
;; 
(provide 'msk-el-get-autoloads)

(defconst msk-el-get-sources (expand-file-name (concat user-emacs-directory user-login-name "-starter-kit/package/msk-el-get-sources.el")))

(defun clone-el-get ()
  "I want el-get without switching to stable branch!
   This is a basic copy of the el-get-install.el script
   that will only branch from the master, not to it"
  (progn
    (let ((el-get-root
           (file-name-as-directory
            (or (bound-and-true-p el-get-dir)
                (concat (file-name-as-directory user-emacs-directory) "el-get")))))

      (when (file-directory-p el-get-root)
        (add-to-list 'load-path el-get-root))

      ;; try to require el-get, failure means we have to install it
      (unless (require 'el-get nil t)
        (unless (file-directory-p el-get-root)
          (make-directory el-get-root t))

        (let* ((package   "el-get")
               (buf       (switch-to-buffer "*el-get bootstrap*"))
               (pdir      (file-name-as-directory (concat el-get-root package)))
               (git       (or (executable-find "git")
                              (error "Unable to find `git'")))
               (url       (or (bound-and-true-p el-get-git-install-url)
                              "http://github.com/dimitri/el-get.git"))
               (default-directory el-get-root)
               (process-connection-type nil)   ; pipe, no pty (--no-progress)

               ;; First clone el-get
               (status
                (call-process
                 git nil `(,buf t) t "--no-pager" "clone" "-v" url package)))
          
          (unless (zerop status)
            (error "Couldn't clone el-get from the Git repository: %s" url))
          
          ;; switch branch if we have to unless requiring `master'
          (unless (and (boundp 'el-get-master-branch)
                       (equal el-get-master-branch "master"))
            (let* ((branch (cond
                            ;; Check if a specific branch is requested
                            ((bound-and-true-p el-get-install-branch))
                            ;; Check if master branch is requested
                            ((boundp 'el-get-master-branch) "master")
                            ;; Read the default branch from the el-get recipe
                            ((plist-get (with-temp-buffer
                                          (insert-file-contents-literally
                                           (expand-file-name "recipes/el-get.rcp" pdir))
                                          (read (current-buffer)))
                                        :branch))
                            ;; As a last resort, use the master branch
                            ("master")))
                   (remote-branch (format "origin/%s" branch))
                   (default-directory pdir)
                   (bstatus
                    (call-process git nil (list buf t) t "checkout" "-t" remote-branch)))
              (unless (zerop bstatus)
                (error "Couldn't `git checkout -t %s`" branch))))
          
          (add-to-list 'load-path pdir)
          (load package)
          (let ((el-get-default-process-sync t) ; force sync operations for installer
                (el-get-verbose t))		    ; let's see it all
            (el-get-post-install "el-get"))
          (with-current-buffer buf
            (goto-char (point-max))))))))

(if (file-exists-p msk-el-get-sources)
  (progn 
    
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

    ;; ;; auto-checkout current stable branch
    ;; (unless (require 'el-get nil t)
    ;;   (with-current-buffer
    ;;       (url-retrieve-synchronously
    ;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    ;;     (end-of-buffer)
    ;;    (eval-print-last-sexp)))

    (unless (require 'el-get nil t)
      (clone-el-get))

    (require 'msk-el-get-sources)
    
    (if (boundp 'msk-el-get-packages)
        (progn
          (el-get 'sync msk-el-get-packages)
          (el-get 'wait)
          ))
))
