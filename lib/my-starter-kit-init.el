;;
(provide 'my-starter-kit-init)

;; my.el will auto-load:
;;   custom/my-init-begin.el
;;   custom/my-init-features.el
;;   custom/*-settings.el
;;   custom/*-hooks.el
;;   custom/my-init-last.el
;;
(setq stack-trace-on-error t)
(setq debug-on-error t)

;; directories we care about
(defvar msk-user-dir (concat user-emacs-directory user-login-name "-starter-kit/"))
(defvar msk-init-dir (concat msk-user-dir "init/"))
(defvar msk-settings-dir (concat msk-user-dir "settings/"))
(defvar msk-hooks-dir (concat msk-user-dir "hooks/"))

(add-to-list 'load-path msk-init-dir)
(add-to-list 'load-path msk-settings-dir)
(add-to-list 'load-path msk-hooks-dir)

;; early init hook
(if (boundp 'msk-init-begin)
  (require 'msk-init-begin))

;; auto-load require packages
(require 'my-package-autoloads)
(require 'my-el-get-autoloads)
(if (boundp 'msk-init-features)
  (require 'msk-init-features))

;; load custom settings
(dolist (setting-file (file-expand-wildcards (concat msk-settings-dir "*-settings.el")))
  (progn (print (concat "---> loading " (file-name-sans-extension (file-name-nondirectory setting-file)))))
  (require (intern (file-name-sans-extension (file-name-nondirectory setting-file)))))

;; load custom hooks
(dolist (hook-file (file-expand-wildcards (concat msk-hooks-dir "*-hooks.el")))
  (progn (print (concat "---> loading " (file-name-sans-extension (file-name-nondirectory hook-file)))))
  (require (intern (file-name-sans-extension (file-name-nondirectory hook-file)))))

(if (boundp 'msk-init-last)
  (require 'msk-init-last))
;;
;; END
;;
