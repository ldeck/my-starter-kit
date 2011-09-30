CHEATSHEET :: Emacs on Rails
============================

References:
* [http://majorursa.net/emacs_cheatsheet.html](http://majorursa.net/emacs_cheatsheet.html)
* [http://cheat.errtheblog.com/s/rinari/](http://cheat.errtheblog.com/s/rinari/)
* [http://sean.wenzel.net/docs/emacs/quick_reference/](http://sean.wenzel.net/docs/emacs/quick_reference/)

Contents
--------

* [Rinari](#rinaricommands)
* [Rails Reloaded Commands](#railsreloadedcommands)
* [ECB Commands](#ecbcommands)
* [Emacs : Help](#emacs:help)
* [Emacs : Editor Commands](#emacs:editorcommands)
* [Emacs : Dired Commands](#emacs:diredcommands)
* [Emacs : ETags Commands](#emacs:etagscommands)
* [Emacs : Tramp Commands](#emacs:trampcommands)
* [Emacs : Magit Commands](#emacs:magitcommands)

Rinari Commands
---------------

    Navigation
    ----------
    C-c ; f c	rinari-find-controller
	C-c ; f e	rinari-find-environment
	C-c ; f f	rinari-find-file-in-project
	C-c ; f h	rinari-find-helper
	C-c ; f i	rinari-find-migration
	C-c ; f j	rinari-find-javascript
	C-c ; f l	rinari-find-plugin
	C-c ; f m	rinari-find-model
	C-c ; f n	rinari-find-configuration
	C-c ; f o	rinari-find-log
	C-c ; f p	rinari-find-public
	C-c ; f r	rinari-find-rspec
	C-c ; f s	rinari-find-script
	C-c ; f t	rinari-find-test
	C-c ; f v	rinari-find-view
	C-c ; f w	rinari-find-worker
	C-c ; f x	rinari-find-fixture
	C-c ; f y	rinari-find-stylesheet
	
	Actions
	--------
	C-c ; c     rinari-console
	C-c ; e     rinari-insert-erb-skeleton
	C-c ; f     Prefix Command
	C-c ; g     rinari-rgrep
	C-c ; q     rinari-sql
	C-c ; r     rinari-rake
	C-c ; s     rinari-script
	C-c ; t     rinari-test
	C-c ; w     rinari-web-server
	C-c ; x     rinari-extract-partial

Rails Reloaded Commands
-----------------------

    TODO

Emacs : Help
----------

    C-h b      =>    describe bindings
    C-h f      =>    describe function
    C-h v      =>    describe variable
    C-h m      =>    describe mode

Emacs : Editor Commands
---------------------

    C-M ;	   =>    comment-region
    C-u C-M ;  =>    un-comment-region
    C-M \	   =>    indent-region
    M-PgUp	   =>    scroll other window up
    M-PgDn	   =>    scroll other window down
    M-%	       =>    queery-replace
    C-x f	   =>    set-column-fill
    M-q	       =>    fill-paragraph

Emacs : Dired Commands
--------------------

    m	mark file
    u	unmark file
    d	mark file for deletion
    x	execute deletions
    Q	regexp search-replace in marked files
    R	rename marked files
    Z	compress file
    ~	mark all backup files for deletion
    #	mark all #'d files for deletion
    O	change owner
    s	toggle sorting by name/date

also C-x C-q allows you to edit the dired directory as a file, and use macros or string-replace to change lots of file names at once. Use C-c C-c to save changes.

Emacs : ETags Commands
----------------------

    M-.	go to function
    M-*	after M-. go back to where you were.
    M-x visit-tags-table	add TAGS file for source
    generate etags	find . -name '*.java' xargs etags

Emacs : Tramp Commands
----------------------

    save file to server	C-x C-w
    use ssh path to save file remotely	/ssh:username@yourserver.org:path/to/file

Emacs : Magit Controls
----------------------

    C-x g	git status
    C-c C-c	when in commit log: commit changes
    **Following commands work in git status buffer**
    s	stage file at point
    u	unstage file at point
    c	open commit log
    P	push master origin untracked file section
    i	add file to ignore
    I	add file to git/info/exclude
    k	in unstaged: resets to head. ie discards changes
    k	in untracked: deletes file
    <tab>	in staged/unstaged: shows/hides diff of changes
    C-u P	push master to ?: prompts for remote to push changes to
    l	git log: hit <enter> on a commit to see changes
    =	diff current with commit at point
    t	create lightweight tag
    T	create annotated tag
    x	prompts for revision resets HEAD to it.

