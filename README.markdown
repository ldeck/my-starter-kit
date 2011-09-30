my-starter-kit
==============

What? Yet another one? Not really, this one makes heavy use of the existing starter kit but provides additional hooks for configuration making it quite simple to get up and running.

What does it do?
----------------

Primarily the motivation for my-starter-kit was my wanting to take what both the official starter-kit
provides, and add to that the ability to auto-configure which packages to additionally include
for your own setup. You simple define what packages you want to be pulled via the package system
and/or those by el-get and voila!

Further-more, it looks for the following files, in order, ignoring those that don't exist

    ~/.emacs.d/<login>-starter-kit/init/msk-init-begin.el
    ~/.emacs.d/<login>-starter-kit/package/msk-package-sources.el
    ~/.emacs.d/<login>-starter-kit/package/msk-el-get-sources.el
    ~/.emacs.d/<login>-starter-kit/init/msk-init-features.el
    ~/.emacs.d/<login>-starter-kit/settings/*-settings.el
    ~/.emacs.d/<login>-starter-kit/hooks/*-hooks.el
    ~/.emacs.d/<login>-starter-kit/init/msk-init-last.el

So it will actually do nothing apart from loading the starter-kit if you don't configure anything!

Step by step guide
------------------

1. download my-starter-kit via git clone

        mkdir -p ~/.emacs.d
        cd ~/.emacs.d
        git clone git://github.com/ldeck/my-starter-kit.git

2. create your ~/.emacs.d/init.el with the following content

        ;; just load my stuff please
        (add-to-list 'load-path (concat (file-name-directory load-file-name) "my-starter-kit"))
        (require 'my-starter-kit)

3. create **~/.emacs.d/<login>-starter-kit/package/msk-package-sources.el** file which defines the packages to automatically pull in via the emacs package facility
    
        ;; define provider for preferred packages
        (provide 'msk-package-sources)
    
        ;; my packages
        (defvar msk-preferred-packages
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
      
          ;; end file

4. create **~/.emacs.d/<login>-starter-kit/package/msk-el-get-sources.el** file which defines the packages to automatically pull in via the el-get facility
    
        ;; define provider for preferred el-get packages
        (provide 'msk-el-get-sources)
    
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
    
        ;; end

5. create **~/.emacs.d/<login>-starter-kit/settings/*-settings.el** files for individual feature configuration. Example files:
    
        my-ecb-settings.el
        my-editor-settings.el
        <etc>

6. create **~/.emacs.d/<login>-starter-kit/hooks/*-hooks.el** files to create an mode hooks you desire

7. Enjoy!

Caveats
-------

Currently the el-get package is asynchronously downloaded (which I need to fix). So it may fail the first time. Just quit emacs and start again once it says el-get has been downloaded.

Feedback
--------

If you find this package for emacs useful or have any suggestions to make it better, feel free to contribute or let me know!
    
    