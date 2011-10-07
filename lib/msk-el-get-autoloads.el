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

(if (file-exists-p msk-el-get-sources)
  (progn 
    
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
    
    (unless (require 'el-get nil t)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
        (end-of-buffer)
        (eval-print-last-sexp)))
    
    (require 'msk-el-get-sources)
    
    (if (boundp 'msk-el-get-packages)
        (progn
          (el-get 'sync msk-el-get-packages)
          (el-get 'wait)
          ))
))



