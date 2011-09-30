; emacs init.el
; assumes emacs >= 24
(setq stack-trace-on-error t)

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
;; All further config goes here:
;; ~/.emacs.d/<login>.el
;; ~/.emacs.d/<login>/*.el
;; ~/.emacs.d/<hostname>.el
;;
;; Further usage here: https://github.com/technomancy/emacs-starter-kit

(add-to-list 'load-path (concat (file-name-directory load-file-name) "lib")
(require 'my-starter-kit-init)

;; Enjoy!

(provide 'my-starter-kit)
