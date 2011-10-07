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

;;
;; ecb-hook-helpers
;;
(defcustom msk-ecb-after-directory-change-toggle-p nil
  "*condition for whether or not to call the function hook
   takes two args DIROLD DIRNEW")

(defcustom msk-ecb-after-directory-change-hook nil
  "*hook to be called in when `msk-ecb-after-directory-change-toggle-p is true
   takes two args DIROLD DIRNEW")

(defun msk-ecb-after-directory-change-hook (dirold dirnew)
  "*hook tied into ecb-after-directory-change-hook
   Requires both msk-ecb-after-directory-change-toggle-p
   and msk-ecb-after-directory-change-hook"
  (let ((diroldfull (expand-file-name (directory-file-name dirold)))
        (dirnewfull (expand-file-name (directory-file-name dirnew))))
    (if (run-hook-with-args-until-success 'msk-ecb-after-directory-change-toggle-p dirold dirnew)
        (run-hook-with-args 'msk-ecb-after-directory-change-hook dirold dirnew))))

(defun msk-ecb-activation-hook ()
  "hook for when ecb is activated"
  (add-hook 'ecb-after-directory-change-hook 'msk-ecb-after-directory-change-hook)
)

(add-hook 'ecb-activate-hook 'msk-ecb-activation-hook)
