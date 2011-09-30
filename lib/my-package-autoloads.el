; load preferred packages via package
(provide 'my-package-autoloads)

(defvar el-get-dir (concat user-emacs-directory "el-get/el-get"))
(defvar msk-el-get-config-dir (concat user-emacs-directory user-login-name "-starter-kit/package/"))

;;
;; nothing to do unless packages are defined in a file
;;
(if (file-exists-p (concat msk-el-get-config-dir "msk-package-sources.el"))

  ;;
  ;; and nothing to do unless the defined packages are defined in a var: msk-preferred-packages
  ;;
  (if (boundp 'msk-preferred-packages)
    
    ;;
    ;; cool, pull them in
    ;;
    (dolist (p msk-preferred-packages)
      (when (not (package-installed-p p))
        (package-install p)))

  ) ;; end if msk-preferred-packages defined
) ;; end if file exists

