;;; msk-rinari-autoloads.el --- lib for my-starter-kit.el
;;
;; Copyright (c) 2011 Lachlan Deck and contributors
;;
;; Author: Lachlan Deck <lachlan.deck@gmail.com>
;; URL: http://github.com/ldeck/my-starter-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This lib file adds an autoloader for rinari from ecb directory
;; changes.

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
(provide 'msk-rinari-autoloads)

(defun msk--rinari-disable ()
  "(private) disables rinari if active"
  (if (and
       (fboundp 'rinari-minor-mode)
       (boundp 'tags-file-name))
      (progn
        (message "msk disabling rinari")
        (makunbound 'tags-file-name)
        (rinari-minor-mode -1))))

(defun msk--rinari-enable (root r-tags-path)
  "(private) enables rinari if not active"
  (progn
    (message "msk enabling rinari")
    (set 'tags-file-name
         (and (file-exists-p r-tags-path) r-tags-path))
    (run-hooks 'rinari-minor-mode-hook)
    (rinari-minor-mode t)))

(defun msk-ecb-based-rinari-launch (dirold dirnew)
  "ECB change dir supplement to the autoloaded rinari-launch function,
   which is only triggered by opening a file into a buffer it would seem.
   Whilst rinari-launch sets a buffer local variable, `tags-file-name,
   this toggle sets or unsets a global variable of the same name and
   toggles the rinari-minor-mode accordingly."
  (if (fboundp 'rinari-root) ; only effective if rinari is bound
      (progn
        (interactive)
        (let* ((root (rinari-root dirnew))
               (r-tags-path (concat root rinari-tags-file-name)))
          (if root
              (msk--rinari-enable root r-tags-path)
            (msk--rinari-disable))))))


(defun msk-ecb-activation-hook ()
  "hook for when ecb is activated"
  (add-hook 'ecb-after-directory-change-hook 'msk-ecb-based-rinari-launch)
)

(defun msk-ecb-deactivation-hook ()
  "hook for when ecb is deactivated"
  (remove-hook 'ecb-after-directory-change-hook 'msk-ecb-based-rinari-launch)
)

(add-hook 'ecb-activate-hook 'msk-ecb-activation-hook)
(add-hook 'ecb-deactivate-hook 'msk-ecb-deactivation-hook)
