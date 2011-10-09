;;; msk-shared-functions.el --- lib for my-starter-kit.el
;;
;; Copyright (c) 2011 Lachlan Deck and contributors
;;
;; Author: Lachlan Deck <lachlan.deck@gmail.com>
;; URL: http://github.com/ldeck/my-starter-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This lib file loads a bunch of useful functions for reusability.

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

(provide 'msk-shared-functions)

(defun msk-parent-file-name (file)
  (cond
   ((not (file-exists-p file)) nil)
   (t
    (let* ((fullpath (directory-file-name (expand-file-name file)))
           (elements (split-string fullpath "/")))
        (cond
         ((equal fullpath "/") nil)
         ((equal (list-length elements) 1) nil)
         ((equal (list-length elements) 2) "/")
         (t
          (mapconcat 'identity (subseq elements 0 -1) "/")))))))

(defun msk-rails-project-root-p (dir)
  (and (file-directory-p dir)
       (or (file-exists-p (concat (file-name-as-directory dir) "app/controllers/application_controller.rb"))
           (file-exists-p (concat (file-name-as-directory dir) "Gemfile"))
           (file-exists-p (concat (file-name-as-directory dir) "Rakefile")))))

(defun msk-rails-project-path-p (dir)
  "walk backwards up DIR executing ACTION with (dir file)"
  (and (not (string= dir nil))
       (not (string= dir ""))
       (file-directory-p dir)
       (or (msk-rails-project-root-p dir)
           (msk-rails-project-path-p (msk-parent-file-name dir)))))

(defun msk-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let (active-modes)
    (mapc (lambda (mode) (condition-case
                        nil
                        (if (and (symbolp mode) (symbol-value mode))
                            (add-to-list 'active-modes mode))
                      (error nil) ))
          minor-mode-list)
    active-modes))

(defun msk-rinari-active-p ()
  "returns true if rinari-minor-mode is currently active"
  (member 'rails-minor-mode (msk-active-modes)))

(defun msk-mode-active-p (mode)
  "true if the symbol MODE is contained in the list returned by msk-active-modes"
  (member mode (msk-active-modes)))


