; load preferred packages via package
(provide 'msk-package-autoloads)

(defconst msk-package-dir (concat user-emacs-directory user-login-name "-starter-kit/package/"))
(defconst msk-package-loader (concat msk-package-dir "msk-package-sources.el"))

(add-to-list 'load-path msk-package-dir)

;;
;; nothing to do unless packages are defined in a file
;;
(if (file-exists-p msk-package-loader)
  (load-file msk-package-loader)
)

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
