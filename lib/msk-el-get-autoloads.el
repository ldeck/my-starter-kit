;; el-get and associated autoloaded packages
;; 
(provide 'msk-el-get-autoloads)

(defvar el-get-dir (concat user-emacs-directory "el-get/el-get"))
(defvar msk-el-get-config-dir (concat user-emacs-directory user-login-name "-starter-kit/package/"))

;;
;; nothing to do unless there's a user config file
;;
(if (file-exists-p (concat msk-el-get-config-dir "msk-el-get-sources.el"))
    
  ;;
  ;; load it
  ;;
  (add-to-list 'load-path msk-el-get-config-dir)
  (require 'msk-el-get-sources)

  ;;
  ;; nothing to do unless the user function 'my-el-get-packages is defined
  ;;
  (if (boundp 'my-el-get-packages)

    ;;
    ;; okay let's grab el-get if it's not already included
    ;;
    (unless (file-exists-p el-get-dir)
      (unless (require 'el-get nil t)
        (url-retrieve
          "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
          (lambda (s)
            (end-of-buffer)
              (eval-print-last-sexp)))))
    
    ;; load el-get
    (add-to-list 'load-path el-get-dir)
    (require 'el-get)

    ;; custom config already loaded
    ;; so we'll pass off to el-get to load stuff as defined
    (el-get 'wait my-el-get-packages)

  ) ; end if my-el-get-packages
) ; end if msk-el-get-config exists
