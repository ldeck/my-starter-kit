; load preferred packages via package
(provide 'msk-package-autoloads)

(defconst elpa-dir (expand-file-name (concat user-emacs-directory "elpa/")))
(defconst msk-package-dir (concat user-emacs-directory user-login-name "-starter-kit/package/"))
(defconst msk-package-loader (concat msk-package-dir "msk-package-sources.el"))

;;
;; nothing to do unless packages are defined in a file
;;
(if (file-exists-p msk-package-loader)
    (progn
      (add-to-list 'load-path msk-package-dir)
      (require 'msk-package-sources)
))

;;
;; and nothing to do unless the defined packages are defined in a var: msk-preferred-packages
;;
(if (boundp 'msk-preferred-packages)
    (progn

      ;; cool, pull them in if not already installed
      (dolist (p msk-preferred-packages)
        (when (not (package-installed-p p))
          (package-install p)))
      
      ;; and add them to the load-path for convenience
      (dolist (p msk-preferred-packages)
        (when (and (package-installed-p p)
                   (not (package-built-in-p p)))
          (let* ((name (symbol-name p))
                 (desc (cdr (assq p package-alist)))
                 (version (package-version-join (package-desc-vers desc))))
            (add-to-list 'load-path (concat elpa-dir name "-" version)))))

      ;; print the load-path
      (progn (print "--> load-path after all preferred packages installed")
             (print load-path))

  ) ; end progn
) ; end if msk-preferred-packages
