my-starter-kit
==============

What? Yet another one for emacs? Not really, this one simply aims to
make it even easier to install packages for emacs on top of the starter-kit.

You simply list what packages you want as described below and fire up
emacs. They're all pulled in as requested from either elpa, el-get or both.

You can mix and match from both el-get and elpa as desired.

What does it do?
----------------

Primarily the motivation for my-starter-kit was my wanting to take
what both the official starter-kit provided, and add the ability to
auto-configure additional packages to be included.

Secondly, I wanted to be able to keep my configuration logically
organised, but without having to add additional hooks in the init.el
file to pull them in. All I wanted to do was drop in another file and
have it picked up automatically (at start). Thus, I can create
my-ecb-settings.el, or my-ruby-hooks.el in the right places and voila!

Where do I put my configuration files?
--------------------------------------

**my-starter-kit** looks for the following files, in the order given,
  ignoring any that don't exist:

    ~/.emacs.d/<login>-starter-kit/init/msk-init-begin.el
    ~/.emacs.d/<login>-starter-kit/package/msk-package-sources.el
    ~/.emacs.d/<login>-starter-kit/package/msk-el-get-sources.el
    ~/.emacs.d/<login>-starter-kit/init/msk-init-features.el
    ~/.emacs.d/<login>-starter-kit/settings/*-settings.el
    ~/.emacs.d/<login>-starter-kit/hooks/*-hooks.el
    ~/.emacs.d/<login>-starter-kit/init/msk-init-last.el

So it will actually do nothing apart from loading the starter-kit if
you don't configure anything! For some, that'll be all you want. When
it comes time, however, to adding more features than is packaged by
default with emacs my-starter-kit provides the easy solution to
automate pulling these in. And, you can then put
~/.emacs.d/<login>-my-starer-kit into version control.

Step by step guide
------------------

1. download my-starter-kit via git clone to anywhere you like. e.g.,

        mkdir -p ~/.emacs.d
        cd ~/.emacs.d
        git clone git://github.com/ldeck/my-starter-kit.git

2. create your ~/.emacs.d/init.el with the following content

        ;; just load my stuff please
        (add-to-list 'load-path (concat user-emacs-directory "my-starter-kit"))
        (require 'my-starter-kit)

3. create
**~/.emacs.d/<login>-starter-kit/package/msk-package-sources.el** file
which defines the packages to automatically pull in via the emacs
package facility
    
        ;; define provider for preferred packages
        (provide 'msk-package-sources)
    
        ;; my packages
        (defconst msk-preferred-packages
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
        (setq msk-el-get-sources
              '((:name ecb
                       :type git
                       :url "git://github.com/emacsmirror/ecb.git"
                              :load "ecb.el")
                (:name rinari
                       :type git
                       :url "git://github.com/eschulte/rinari.git")))

        (setq msk-el-get-packages
              (append
               '(el-get rinari ecb)
               (mapcar 'el-get-source-name msk-el-get-sources)))
    
        ;; end

5. create **~/.emacs.d/<login>-starter-kit/settings/*-settings.el**
files for individually *provide*d per feature configurations. Example files:
    
        my-ecb-settings.el
        my-editor-settings.el
        <etc>

6. create **~/.emacs.d/<login>-starter-kit/hooks/*-hooks.el** files to
create any mode hooks you desire

7. Enjoy!

Caveats
-------

Currently the el-get package is asynchronously downloaded (which I need to fix). So it may fail the first time. Just quit emacs and start again once it says el-get has been downloaded.

Feedback
--------

If you find this package for emacs useful or have any suggestions to make it better, feel free to contribute or let me know!
    
    
