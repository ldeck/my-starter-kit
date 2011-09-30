;; el-get and associated autoloaded packages
;; 
(provide 'my-el-get-autoloads)

;;
;; install el-get if not already installed
(unless (file-exists-p "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil t)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)))))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; define additional packages sources to install via el-get
(setq el-get-sources
      '((:name ecb
               :type git
               :url "git://github.com/emacsmirror/ecb.git"
                      :load "ecb.el")
        (:name rinari
               :type git
               :url "git://github.com/eschulte/rinari.git")))

(setq my-el-get-packages
      (append
       '(el-get rinari ecb)
       (mapcar 'el-get-source-name el-get-sources)))

;; update them if necessary, wait for it...
(el-get 'wait my-el-get-packages)
