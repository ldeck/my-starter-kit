;;
;; init.el
;;
(setq stack-trace-on-error t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq x-select-enable-clipboard t)

;;
;; Manually customed variables
;; not sure of the order
;;
(push "/usr/local/bin" exec-path)
(push "/opt/local/bin" exec-path)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/open")

(defun google-region (beg end)
  "Google the selected region."
  (interactive "r")
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" (buffer-substring beg end))))

(global-set-key (kbd "C-c C-g C-i") 'google-region) ; [g]oogle [i]t

;;
;; editing preferences
;;
(setq-default tab-width 2)              ; tabs
(setq-default indent-tabs-mode nil)
(global-font-lock-mode t)               ; syntax highlighting
(setq font-lock-maximum-decoration t)   ; mode default level of fontification
(delete-selection-mode t)
(blink-cursor-mode t)
(show-paren-mode t)
(line-number-mode t)
(setq linum-format " %4d ")
(global-linum-mode t)
(column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(follow-mode t)                         ; Easier editing of longs files

(require 'hl-line)                      ; highlight current line
(global-hl-line-mode t)
;(highlight-current-line-set-bg-color "#E5F5B3")

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t)
;(cua-selection-mode t) ; for standard emacs key-commands


;;
;; files / history
;;
(require 'recentf)
(recentf-mode t)

;;
;; editing hooks
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; packages are available from a few places:
;; @see http://www.emacswiki.org/emacs/ELPA
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmelaide" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)

;; ECB preferences early setup
(setq ecb-auto-activate 't)
(setq ecb-tip-of-the-day nil)
(setq ecb-layout-window-sizes (quote (("left8" (ecb-directories-buffer-name 0.25 . 0.28) (ecb-sources-buffer-name 0.25 . 0.23) (ecb-methods-buffer-name 0.25 . 0.28) (ecb-history-buffer-name 0.25 . 0.17)))))

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
)

;; Enable the CEDET Project management system
(require 'cedet)
(semantic-mode t)
(global-ede-mode t)

;; xemacs specific
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode t))
(if (fboundp 'set-fringe-style)
    (set-fringe-style -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'tooltip-mode)
    (tooltip-mode -1))

;; ECB
(add-to-list 'load-path (expand-file-name (concat (file-name-directory load-file-name) "/vendor/ecb")))
(require 'ecb)

;;uncomment these momentarily if ecb fails to load
;;(ecb-activate)
;;(ecb-byte-compile)

;; theme
;;(if (fboundp 'load-theme)
;;    (load-theme 'tango))
(if (fboundp 'set-frame-font)
    (set-frame-font "Menlo-12"))

;; transparency
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;;(set-frame-parameter (selected-frame) 'alpha '(90 50))
;;(add-to-list 'default-frame-alist '(alpha 90 50))

;; toggle transparency
;; (defun set-frame-alpha (arg &optional active)
;;   (interactive "nEnter alpha value (1-100): \np")
;;   (let* ((elt (assoc 'alpha default-frame-alist))
;;          (old (frame-parameter nil 'alpha))
;;          (new (cond ((atom old)     `(,arg ,arg))
;;                     ((eql 1 active) `(,arg ,(cadr old)))
;;                     (t              `(,(car old) ,arg)))))
;;     (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
;;     (set-frame-parameter nil 'alpha new)))
;; (global-set-key (kbd "C-c t") 'set-frame-alpha)

;; ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t)

(smex-initialize)                       ; ido for M-x
(global-set-key (kbd "M-x") 'smex)

;;
;; Various Modes for Ruby / Rails Development
;; ------------------------------------------
;;

;; rinari :: https://github.com/eschulte/rinari
(add-to-list 'load-path (expand-file-name (concat (file-name-directory load-file-name) "/vendor/rinari")))
(require 'rinari)

;; textmate mode :: git://github.com/defunkt/textmate.el
(add-to-list 'load-path (expand-file-name (concat (file-name-directory load-file-name) "/vendor/textmate")))
;(add-to-list 'load-path (expand-file-name "./vendor/textmate"))
(require 'textmate)
(textmate-mode)

;; emacs-rails-reloaded :: https://github.com/r0man/emacs-rails-reloaded
(add-to-list 'load-path (expand-file-name (concat (file-name-directory load-file-name) "/vendor/emacs-rails-reloaded")))
(require 'rails-autoload)

;; cucumber support :: https://github.com/michaelklishin/cucumber.el
(add-to-list 'load-path (expand-file-name (concat (file-name-directory load-file-name) "/vendor/cucumber")))
(setq feature-default-language "en")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

