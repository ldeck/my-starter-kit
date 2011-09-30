; emacs init.el
(provide 'my-starter-kit)

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
(require 'my-starter-kit-init)

;; Enjoy!
