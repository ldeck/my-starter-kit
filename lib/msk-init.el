;; my-starter-kit-init loads the <login>-starter-kit defined configuration
;; see README for docs
;; 
(provide 'msk-init)

(setq stack-trace-on-error t)
(setq debug-on-error t)

;; add directories we care about to the load-path
(defconst msk-user-dir (concat user-emacs-directory user-login-name "-starter-kit/"))
(defconst msk-init-dir (concat msk-user-dir "init/"))
(defconst msk-settings-dir (concat msk-user-dir "settings/"))
(defconst msk-hooks-dir (concat msk-user-dir "hooks/"))

(add-to-list 'load-path msk-init-dir)
(add-to-list 'load-path msk-settings-dir)
(add-to-list 'load-path msk-hooks-dir) 

;; load user defined msk-init-begin if available
(if (boundp 'msk-init-begin)
  (require 'msk-init-begin))

;; package and el-get autoloads manage themselves
(add-to-list 'load-path (expand-file-name (concat msk-user-dir "package")))
(require 'msk-package-autoloads)
(require 'msk-el-get-autoloads)

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
