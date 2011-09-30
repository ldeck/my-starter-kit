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
(defvar my-starter-kit-init-dir (concat (user-emacs-directory user-login-name "/init")))
(defvar my-starter-kit-settings-dir (concat (user-emacs-directory user-login-name "/settings")))
(defvar my-starter-kit-hooks-dir (concat (user-emacs-directory user-login-name "/hooks")))

(add-to-list 'load-path my-starter-kit-init-dir)
(add-to-list 'load-path my-starter-kit-settings-dir)
(add-to-list 'load-path my-starter-kit-hooks-dir)

;; auto-load require packages
(require 'my-package-autoloads)
(require 'my-el-get-autoloads)

;; load custom settings
(dolist (setting-file (file-expand-wildcards (concat my-settings-dir "*-settings.el")))
  (progn (print (concat "---> loading " (file-name-sans-extension (file-name-nondirectory setting-file)))))
  (require (intern (file-name-sans-extension (file-name-nondirectory setting-file)))))

;; load custom hooks
(dolist (hook-file (file-expand-wildcards (concat my-hooks-dir "*-hooks.el")))
  (progn (print (concat "---> loading " (file-name-sans-extension (file-name-nondirectory hook-file)))))
  (require (intern (file-name-sans-extension (file-name-nondirectory hook-file)))))

(require 'my-init-last)
;;
;; END
;;
