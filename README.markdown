my-starter-kit
==============

What? Yet another one for emacs? Not really, this one simple aims to make it even easier to install the others as well as the various packages for emacs that are out there in the wild.

You simply list what you want as described below and fire up emacs. They're all pulled in as requested.

There are two mechanisms and possible sources for your packages: either (a) elpa, or (b) el-get. You can mix and match from the two as desired in order to customise your setup.

What does it do?
----------------

Primarily the motivation for my-starter-kit was my wanting to take what both the official starter-kit
provides, and add to that the ability to auto-configure which packages to additionally include
for your own setup. You simple define what packages you want to be pulled via the package system
and/or those by el-get and voila!

**my-starter-kit** looks for the following files, in the order given, for provided features and ignoring any that don't exist:

    ~/.emacs.d/<login>-starter-kit/init/msk-init-begin.el
    ~/.emacs.d/<login>-starter-kit/package/msk-package-sources.el
    ~/.emacs.d/<login>-starter-kit/package/msk-el-get-sources.el
    ~/.emacs.d/<login>-starter-kit/init/msk-init-features.el
    ~/.emacs.d/<login>-starter-kit/settings/*-settings.el
    ~/.emacs.d/<login>-starter-kit/hooks/*-hooks.el
    ~/.emacs.d/<login>-starter-kit/init/msk-init-last.el

So it will actually do nothing apart from loading the starter-kit if you don't configure anything! For some, that'll be all you want. When it comes time, however, to adding more features than is packaged by default with emacs my-starter-kit provides the easy solution to automate pulling these in. And, you can then put ~/.emacs.d/<login>-my-starer-kit into version control.lr

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
    
    