;;; msk-package-autoloads.el --- lib for my-starter-kit.el
;;
;; Copyright (c) 2011 Lachlan Deck and contributors
;;
;; Author: Lachlan Deck <lachlan.deck@gmail.com>
;; URL: http://github.com/ldeck/my-starter-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This lib file loads the user defined package.el sources.

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


; load preferred packages via package
(provide 'msk-package-autoloads)

(defconst elpa-dir (expand-file-name (concat user-emacs-directory "elpa/")))
(defconst msk-package-dir (concat user-emacs-directory user-login-name "-starter-kit/package/"))
(defconst msk-package-loader (concat msk-package-dir "msk-package-sources.el"))

;;
;; nothing to do unless packages are defined in a file
;;
(if (file-exists-p msk-package-loader)
    (progn
      (add-to-list 'load-path msk-package-dir)
      (require 'msk-package-sources)
))

;;
;; and nothing to do unless the defined packages are defined in a var: msk-preferred-packages
;;
(if (boundp 'msk-preferred-packages)
    (progn

      ;; cool, pull them in if not already installed
      (dolist (p msk-preferred-packages)
        (when (not (package-installed-p p))
          (package-install p)))
      
      ;; and add them to the load-path for convenience
      (dolist (p msk-preferred-packages)
        (when (and (package-installed-p p)
                   (not (package-built-in-p p)))
          (let* ((name (symbol-name p))
                 (desc (cdr (assq p package-alist)))
                 (version (package-version-join (package-desc-vers desc))))
            (add-to-list 'load-path (concat elpa-dir name "-" version)))))

      ;; print the load-path
      (progn (print "--> load-path after all preferred packages installed")
             (print load-path))

  ) ; end progn
) ; end if msk-preferred-packages
