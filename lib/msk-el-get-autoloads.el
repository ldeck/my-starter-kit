;; el-get and associated autoloaded packages
;; 
(provide 'msk-el-get-autoloads)

(defconst msk-el-get-sources (expand-file-name (concat user-emacs-directory user-login-name "-starter-kit/package/msk-el-get-sources.el")))

(if (file-exists-p msk-el-get-sources)
  (progn 
    
    (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
    
    (unless (require 'el-get nil t)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
        (end-of-buffer)
        (eval-print-last-sexp)))
    
    (require 'msk-el-get-sources)
    
    (if (boundp 'msk-el-get-packages)
        (progn
          (el-get 'sync msk-el-get-packages)
          (el-get 'wait)
          ))
))



