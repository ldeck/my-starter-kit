;;; my-starter-kit.el --- Easy defaults and goodies.
;;
;; Copyright (c) 2011 Lachlan Deck and contributors
;;
;; Author: Lachlan Deck <lachlan.deck@gmail.com>
;; URL: http://github.com/ldeck/my-starter-kit
;; Version: 1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is the master file to require to take advantage of my-starter-kit.

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

;
; assumes emacs >= 24
;
(provide 'my-starter-kit)

(unless (boundp 'stack-trace-on-error)
  (setq stack-trace-on-error nil))

;; packages are available from a few places:
;; @see http://www.emacswiki.org/emacs/ELPA
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmelaide" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;
;; minimum requirement: starter-kit
;;
(defvar init-packages '(starter-kit)
  "A list of packages to ensure are installed for an initial launch.")

(dolist (p init-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;
;; All further config for the standard starter-kit goes here:
;; ~/.emacs.d/<login>.el
;; ~/.emacs.d/<login>/*.el
;; ~/.emacs.d/<hostname>.el
;;
;; Further usage here: https://github.com/technomancy/emacs-starter-kit

;;
;; But you can provide your configs using the my-starter-kit way too/instead
;; ~/.emacs.d/<login>-starter-kit/package/msk-el-get-sources.el
;; ~/.emacs.d/<login>-starter-kit/package/msk-package-sources.el
;; ~/.emacs.d/<login>-starter-kit/settings/*-settings.el
;; ~/.emacs.d/<login>-starter-kit/hooks/*-hooks.el
;;

(add-to-list 'load-path (concat (file-name-directory load-file-name) "lib"))
(require 'msk-init)

;; Enjoy!
