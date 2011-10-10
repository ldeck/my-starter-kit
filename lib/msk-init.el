;;; msk-init.el --- lib for my-starter-kit.el
;;
;; Copyright (c) 2011 Lachlan Deck and contributors
;;
;; Author: Lachlan Deck <lachlan.deck@gmail.com>
;; URL: http://github.com/ldeck/my-starter-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This lib file is the main worker for my-starter-kit.el

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

;; my-starter-kit-init loads the <login>-starter-kit defined configuration
;; see README for docs
;; 
(provide 'msk-init)

(unless (boundp 'stack-trace-on-error)
  (setq stack-trace-on-error nil))
(unless (boundp 'debug-on-error)
  (setq debug-on-error nil))

;; add directories we care about to the load-path
(defconst msk-user-dir (concat user-emacs-directory user-login-name "-starter-kit/"))
(defconst msk-init-dir (concat msk-user-dir "init/"))
(defconst msk-settings-dir (concat msk-user-dir "settings/"))
(defconst msk-hooks-dir (concat msk-user-dir "hooks/"))
(defconst msk-themes-dir (concat msk-user-dir "themes/"))

(add-to-list 'load-path msk-init-dir)
(add-to-list 'load-path msk-settings-dir)
(add-to-list 'load-path msk-hooks-dir)
(add-to-list 'load-path msk-themes-dir)

;; load user defined msk-init-begin if available
(if (boundp 'msk-init-begin)
  (require 'msk-init-begin))

(require 'msk-shared-functions)

;; package and el-get autoloads manage themselves
(add-to-list 'load-path (expand-file-name (concat msk-user-dir "package")))
(require 'msk-package-autoloads)
(require 'msk-el-get-autoloads)

;; autoloads for particular modes
(require 'msk-rinari-autoloads)

;; set file to save/load emacs customisations to
;; ~/.emacs.d/<login>-starter-kit/msk-custom.el
;; and load it if it exists
(unless custom-file
  (setq custom-file (concat msk-user-dir "msk-custom.el"))
  (if (file-exists-p custom-file)
      (load custom-file))
)

;; load user defined msk-init-features if available
(if (boundp 'msk-init-features)
  (require 'msk-init-features))

;; load any user defined custom settings
(dolist (setting-file (file-expand-wildcards (concat msk-settings-dir "*-settings.el")))
  (progn (print (concat "---> loading " (file-name-sans-extension (file-name-nondirectory setting-file)))))
  (require (intern (file-name-sans-extension (file-name-nondirectory setting-file)))))

;; load any user defined custom hooks
(dolist (hook-file (file-expand-wildcards (concat msk-hooks-dir "*-hooks.el")))
  (progn (print (concat "---> loading " (file-name-sans-extension (file-name-nondirectory hook-file)))))
  (require (intern (file-name-sans-extension (file-name-nondirectory hook-file)))))

;; finally load user defined msk-init-last if available
(if (boundp 'msk-init-last)
  (require 'msk-init-last))
;;
;; END
;;
