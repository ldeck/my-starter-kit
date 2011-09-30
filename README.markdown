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

