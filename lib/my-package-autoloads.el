; load preferred packages via package
(provide 'my-package-autoloads)

;;
;; grab these packages automatically
;; TODO: allow list of custom packages read from file.
;;
(defvar my-starter-kit-packages
  '(starter-kit
    smex
    inf-ruby
    ruby-compilation
    starter-kit-ruby
    css-mode
    textmate
    cedet
    ido
    jump)
  "My required packages for launch.")

(dolist (p my-starter-kit-packages)
  (when (not (package-installed-p p))
    (package-install p)))
