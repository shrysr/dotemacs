
# Table of Contents

1.  [Introduction](#orge9bdbfd)
2.  [This configuration](#org7080338)
        1.  [Using this configuration](#org73d0e68)
3.  [Other literate Emacs configs](#orga85d313)
4.  [Tangle org mode config on save](#org6760f68)
5.  [OS Level variables <code>[0/0]</code>](#orga272432)
6.  [PDF Tools](#org01c35e6)
7.  [Better defaults](#org1263d20)
8.  [Crypto setup](#org55eedd7)
    1.  [github token access](#orga653560)
9.  [Emacs General config](#orgeb96647)
    1.  [Remove trailing whitespace at the end of lines](#org375dce4)
    2.  [Remove '^' at the start of ivy commands](#org2e50293)
    3.  [Package installation](#orgc9dd9ed)
        1.  [Package list](#orgcf7b866)
        2.  [Fetch and install missing packages](#org770c3ff)
    4.  [Switch-window configuration](#orgcec4bee)
    5.  [Create intermediate directories while saving files](#orgca9318a)
    6.  [Shorcuts and registers](#org7a347c0)
        1.  [Registers](#org4ea25da)
        2.  [Google this](#orgf8b6144)
        3.  [ivy-yasnippet](#orge065622)
        4.  [Mu4e related](#orgc8d7829)
        5.  [Org related](#org85e1734)
    7.  [yanking links in org format](#org70eb956)
    8.  [Export setup](#orgce6f47d)
    9.  [Markdown config](#org11e865e)
    10. [Export async](#org4f84f7d)
    11. [Ob-async](#orgdf06486)
    12. [Completed loading message](#orga5f77c3)
10. [Crux  - basic movement](#orga962dce)
11. [Dired](#orgbfed7f0)
12. [Swiper](#orgfacf8c1)
13. [Easier selection](#org4ba2d61)
    1.  [Expand region](#orge0d780d)
14. [git related](#orga45c69a)
    1.  [Git gutter](#orgaca43d2)
    2.  [magit settings](#org13a0da1)
    3.  [Time machine for git](#org52cdc3f)
    4.  [Completed loading message](#org923aeb6)
15. [Writeroom customisations](#orgac6c04c)
16. [ESS configuration <code>[/]</code>](#orgea18070)
    1.  [Set condition and path for TAD on Linux](#org4d5908f)
17. [lintr](#orgd9ec89c)
18. [Polymode](#org6b602a5)
19. [Multiple Cursors](#org1530dcc)
20. [ox-reveal - presentations](#org61b8ef7)
21. [Org-mode related](#org1b9feb9)
    1.  [Default org directory and setting it as the agenda file directory](#org51f9c5e)
    2.  [Org-notes into log drawer](#orga4f4f81)
    3.  [Enabling org capture and org protocol](#org9e1e554)
    4.  [Ensuring archive files are also in org mode](#orgef7a4a9)
    5.  [Archiving mechanics](#org65c8413)
    6.  [Org journal](#org1dd12bc)
        1.  [Figure out easy encryption approach for org journal](#org6162530)
    7.  [Use Org ID for storing objects](#orgdd7a8c6)
    8.  [Setting custom keywords with fast access](#org15ee8d0)
    9.  [Cosmetics for org](#org7cae1c4)
        1.  [Basic cosmetics. Review & Convert to use-package style](#org88cfecd)
        2.  [Setting font faces for headline level](#org51e1cd9)
        3.  [Striking out Done headlines](#orgc683e02)
        4.  [Formatting keywords as boxes with inverted colors](#org6c58420)
    10. [Refiling settings](#org5a481c4)
        1.  [Refile target level for search](#org8dae0b9)
        2.  [General refile settings](#orgb943829)
        3.  [Also refer Refiling hydra](#org86733e9)
    11. [Agenda mechanics](#orgfe80a43)
        1.  [Weekday starts on Monday](#org1d4223c)
        2.  [Display heading tags farther to the right](#org7b277ca)
        3.  [Agenda customisation](#org731c5bd)
        4.  [Expanding search locations](#orgb4307f8)
        5.  [Enable default fuzzy search like in google](#org8214bc9)
        6.  [Enable sticky agenda](#org8cdb53e)
        7.  [org-habit](#org583c5b1)
    12. [Capture mechanics](#orgaf205c1)
        1.  [Capture templates](#orgf6c1d2c)
        2.  [Hooks for capture frame control](#org641a0d8)
    13. [version control and backup of files](#org9e5df58)
    14. [Include gpg files in agenda generation](#org6eba7a5)
    15. [org-noter](#org24fe593)
    16. [Persp-projectile](#org1cc9bb9)
    17. [org-projectile](#org8eb6349)
    18. [org-gcal customisation](#org558a06b)
    19. [Property customisation](#org7a67285)
        1.  [Add a CREATED property for org heading](#org71e3af3)
        2.  [Enabling adding tags in the capture window](#orgbb536df)
    20. [org web clipper](#org6567e1f)
    21. [Org-babel](#orge944bb1)
        1.  [Loading language base](#orgf4a1faf)
        2.  [Clojure and cider](#org50d0989)
    22. [Org-trello](#org45e8402)
    23. [Loading completed](#orgb9f859c)
22. [Helm](#org4c7c85f)
    1.  [Setting Helm to be used for specific functions](#org45e8ffe)
    2.  [Persistent follow mode for Helm](#org29ed532)
    3.  [Setting sources for helm](#orgca96cbd)
    4.  [helm-ag and helm-org-rifle](#orge65ce5c)
    5.  [helm-swoop](#orgf56ffc0)
    6.  [Loading completed](#org0d0be7d)
23. [Flycheck](#org945af6f)
    1.  [Basic config](#org18446fe)
    2.  [Replacing flycheck with flymake](#org3f30965)
24. [Scheme setup](#org49e7bf1)
25. [Hydras and some custom functions](#org577ba8d)
    1.  [Refiling](#orgac86ce6)
    2.  [Window manipulation](#org669d38a)
    3.  [helm-do-ag in specific locations](#org14502d4)
        1.  [In project directory](#org7b8bbeb)
        2.  [Scimax config directory](#orgce9c3a7)
        3.  [Journal directory](#orgbf2a6e0)
        4.  [BGR file](#org58d91fa)
        5.  [Defining hydra](#orgf9318c6)
    4.  [Frame configurations fo magit and project launch](#org1bc58a8)
        1.  [Scimax - magit and windows](#orgac3a010)
        2.  [Org files - magit and windows](#orgf7fe94c)
        3.  [Project directory - magit and windows](#org3d5ceb6)
        4.  [Project: Switch and windows](#org8ccc4ae)
        5.  [Defining Hydra](#org4c7d57e)
    5.  [Loading completed](#org53523d1)
26. [Elfeed customisation](#org378cec0)
    1.  [Elfeed Basic + Customisations](#orgbd8d288)
    2.  [Elfeed-org and elfeed-goodies setup <code>[/]</code>](#org1f48e7d)
    3.  [Functions to support syncing](#orgebf7de8)
    4.  [Consider storing the Feed sources here in org format](#org5f6495b)
    5.  [Loading completed](#org27083ab)
27. [w3m customisation](#org6378b32)
    1.  [Appending HTTP to web addresses entered by hand](#orge4ae54f)
    2.  [Changing w3m shortcuts for better tabbed browsing](#org1b90cfb)
    3.  [Default external browser settings](#org0ece42b)
    4.  [Wikipedia search](#orga65ebe1)
    5.  [Access Hacker News](#org45e689c)
28. [ediff](#orgf4e1456)
29. [Theme and visuals](#org3f0ebe6)
    1.  [Emacsclient or frame specific settings](#orgb222ff5)
    2.  [Custom Safe themes and Background change to light grey](#orgbea9578)
    3.  [Font Customisation based on OS](#org31e27c6)
    4.  [visual-fill-column](#org8797f53)
30. [Hugo](#org587f7ce)
    1.  [Function to create specific properties for a blog post](#org810c14e)
        1.  [Defining content directory](#org587bf4b)
        2.  [Ensuring properties exist and creating if they dont exist](#orgbfc8562)
        3.  [Hugo function calling the above](#orge5288d5)
    2.  [ox-hugo setup](#orgc531031)
31. [Scimax customisations](#orgdc58a72)
    1.  [Scimax Hotspots](#org69220a5)
    2.  [Scimax Elfeed](#orgdcef3a7)
    3.  [Scimax Notebook directory](#org816c62c)
    4.  [Scimax notebook](#orgef72ee1)
    5.  [Scimax Python](#org0bb7f26)
    6.  [Bibliography settings and customisation](#org88f4557)
32. [Python <code>[0/4]</code>](#org4b969da)
    1.  [Using miniconda](#org2f76795)
    2.  [setup virtual environment approach](#org9ba170b)
    3.  [setup conda, especially for auto complete](#org9870a98)
    4.  [General config](#orged42586)
    5.  [Autocomplete for python blocks](#org5d72539)
    6.  [Emacs-jupyter](#org40db753)
        1.  [Test for jupyter-python](#orgd95469f)
33. [Project publishing setup <code>[0/2]</code>](#orgf194008)
    1.  [Exporting org projects](#orgc661e56)
    2.  [Function for exporting dotemacs config <code>[1/3]</code>](#org9b3bbaf)
34. [mu4e](#org49ef83b)



<a id="orge9bdbfd"></a>

# Introduction

This is my literate, Org-mode based configuration for Emacs, which are essentially customisations built on top of the starter-kit Scimax. View a nicely rendered version with easy navigation [on my website](https://shrysr.github.io/docs/sr-config/), or if you prefer: [on github](https://github.com/shrysr/dotemacs).

> Scimax - Awesome editing for scientists and engineers. Scimax is an Emacs starterkit for scientists and engineers. It provides a comprehensive configuration of Emacs for scientific programming and publishing.
>
> [John Kitchin](https://github.com/jkitchin)

Scimax specific variables have their own heading to make it 'easier' to experiment with other starter-kits.

The style of documentation is particularly influenced by the [dotemacs config](https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org) of [Mathieu Marques](https://github.com/angrybacon), which I found very engaging to read.

> Note: The configuration posted on my website and github repo are updated from time to time, and may be older than the version I am using everyday.


<a id="org7080338"></a>

# This configuration

Scimax's init calls the `user.el` script placed in the user folder. The following snippet is placed in `user.el` to load this org file and then my encrypted personal configuration. This org file and the tangled emacs-lisp script is also available in a [github repo](https://github.com/shrysr/dotemacs).

    ;; Adding some external packages like reveal for presentations
    (add-to-list 'load-path "~/scimax/user/external_packages/")

    ;; Loading this file that you are viewing, which I name sr-config.org
    (org-babel-load-file (expand-file-name "sr-config.org" user-emacs-directory))

    ;; Loading secret config containing personal information
    (org-babel-load-file (expand-file-name "sr-secrets.org.gpg" user-emacs-directory))

    (garbage-collect)


<a id="org73d0e68"></a>

### TODO Using this configuration

You may need to set `:tangle no` in the headers for the code snippets that you do not need, and set the location of directories for org files, org agenda etc.

1.  Method 1

    1.  Clone Scimax
    2.  Add the above snippet to `user.el` in the user directory. Update the file name and paths as required.
    3.  Place this org file in the user directory.
    4.  Run the provided script for installing the packages needed for Scimax. Once that is done, `user.el` will call this org file.


<a id="orga85d313"></a>

# Other literate Emacs configs

These references were used for exploration and inspiration.

1.  [Karl Voit](https://karl-voit.at/2017/06/03/emacs-org/)
2.  [Mathieu Marques](https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org)
3.  [Lee Hinman](https://writequit.org/org/)
4.  [Sacha Chua](http://pages.sachachua.com/.emacs.d/Sacha.html)


<a id="org6760f68"></a>

# TODO Tangle org mode config on save

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-14 Thu 13:14] </span></span>
    Need to add a condition of check: tangle if the file does not exist.

Source: <https://thewanderingcoder.com/2015/02/literate-emacs-configuration/>

This is a nice code snippet to automate the tangling on saving the config. This saves time while starting up Emacs&#x2026;

    (defun sr/tangle-on-save-emacs-config-org-file()
      (interactive)
      (if (string= buffer-file-name (file-truename "~/scimax/user/sr-config.org"))
          (org-babel-tangle-file  "~/scimax/user/sr-config.org" "~/scimax/user/sr-config.el")
        )
      )

    (defun sr/tangle-if-file-absent ()
      (interactive)
      (if nil  (file-exists-p "~/scimax/user/sr-config.el")
        (org-babel-tangle-file  "~/scimax/user/sr-config.org" "~/scimax/user/sr-config.el")
        )
      )
    ;; (add-hook 'after-save-hook 'sr/dotemacs-export)
    (add-hook 'after-save-hook
              'sr/tangle-on-save-emacs-config-org-file)


<a id="orga272432"></a>

# OS Level variables <code>[0/0]</code>

Since I switch between a Linux machine and a Mac frequently, it is better to define variables that can be used to set other variables depending on the OS.

    ;; Get current system's name
    (defun insert-system-name()
      (interactive)
      "Get current system's name"
      (insert (format "%s" system-name))
      )

    ;; Get current system type
    (defun insert-system-type()
      (interactive)
      "Get current system type"
      (insert (format "%s" system-type))
      )

    ;; Check if system is Darwin/Mac OS X
    (defun system-type-is-darwin ()
      (interactive)
      "Return true if system is darwin-based (Mac OS X)"
      (string-equal system-type "darwin")
      )

    ;; Check if system is GNU/Linux
    (defun system-type-is-gnu ()
      (interactive)
      "Return true if system is GNU/Linux-based"
      (string-equal system-type "gnu/linux")
      )
    (message "Completed OS Level variables load")


<a id="org01c35e6"></a>

# PDF Tools

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-18 Mon 14:30] </span></span>
    Install epdfinfo via 'brew install pdf-tools' and then install the
    pdf-tools elisp via the use-package below. To upgrade the epdfinfo
    server, use 'brew upgrade pdf-tools' prior to upgrading to newest
    pdf-tools package using Emacs package system. If things get messed up,
    just do 'brew uninstall pdf-tools', wipe out the elpa pdf-tools
    package and reinstall both as at the start.  source:
    <https://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx>

    (use-package pdf-tools
      :ensure t
      :config
      (custom-set-variables
       '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead in the mac
      (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
      (pdf-tools-install)
    )


<a id="org1263d20"></a>

# Better defaults

I need to explore the changed made by this package. For now, it is loaded right in the beginning so that it does not overwrite other customisations down the line.

    (use-package better-defaults
      :ensure t
    )

    (message "Loaded better-defaults package")


<a id="org55eedd7"></a>

# Crypto setup

    (setq epa-file-encrypt-to '("shreyas@fastmail.com"))
    (require 'org-crypt)
    (add-to-list 'org-modules 'org-crypt)
                                            ; Encrypt all entries before saving
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                            ; GPG key to use for encryption. nil for symmetric encryption
    (setq org-crypt-key nil)
    (setq org-crypt-disable-auto-save t)
    (setq org-crypt-tag-matcher "locked")

    (message "Loaded crypto setup")


<a id="orga653560"></a>

## github token access

Source: <https://emacs.stackexchange.com/questions/40994/using-auth-source-with-magit-and-bitbucket>

Fill the out the following details before executing the script. Machine can be found be executing 'hostname' in shell.

    cat > ~/.gh.authinfo << EOF
    machine shrysr@github.com password ABCD
    EOF

`M-x epa-encrypt-file` and point towards the above file and choose your key. This will generate the .gpg file.

    (setq auth-sources '((:source "~/.gh.authinfo.gpg")))
    (setq magit-process-find-password-functions '(magit-process-password-auth-source))


<a id="orgeb96647"></a>

# Emacs General config


<a id="org375dce4"></a>

## Remove trailing whitespace at the end of lines

    (add-hook 'before-save-hook 'delete-trailing-whitespace)


<a id="org2e50293"></a>

## Remove '^' at the start of ivy commands

    (setq ivy-initial-inputs-alist nil)


<a id="orgc9dd9ed"></a>

## Package installation


<a id="orgcf7b866"></a>

### Package list

Though the use-package approach is a lot more elegant, I also like to have a list of all my installed packages. In any case, this is more in line with my earlier configurations. As things evolve, I will probably shift to the use-package method.

    (setq package-list '(diminish
                         ztree
                         org-gcal
                         w3m
                         org-trello
                         org-web-tools
                         auto-indent-mode
                         ob-sql-mode
                         dash
                         org-super-agenda
                         workgroups2
                         switch-window
                         ess
                         ess-R-data-view
                         interleave
                         deft
                         org-bookmark-heading
                         writeroom-mode
                         evil
                         evil-leader
                         polymode
                         poly-R
                         helm-ag
                         writegood-mode
                         artbollocks-mode
                         multiple-cursors
                         ox-reveal
                         better-defaults
                         jedi jedi-core
                         ag ein
                         ein-mumamo
                         ido-vertical-mode
                         company-jedi
                         conda
                         spacemacs-theme
                         elfeed-goodies
                         helpful
                         browse-kill-ring
                         ivy-yasnippet
                         speed-type
                         clojure-mode
                         cider
                         helm-dash
                         org-projectile
                         bash-completion
                         elmacro
                         helm-org-rifle
                         sx define-word))


<a id="org770c3ff"></a>

### Fetch and install missing packages

    ;;fetch the list of packages available
    (unless package-archive-contents
      (package-refresh-contents))

    ;; install the missing packages
    (dolist (package package-list)
      (unless (package-installed-p package)
        (package-install package)))


<a id="orgcec4bee"></a>

## Switch-window configuration

Source link: <https://github.com/dimitri/switch-window>

    (use-package switch-window
      :config
      ;;

      (require 'switch-window)

      (global-set-key (kbd "C-x o") 'switch-window)
      (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
      (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
      (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
      (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

      (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
      (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
      (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
      (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

      (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
      (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

      (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)

      ;; selecting minibuffer
      (setq switch-window-minibuffer-shortcut ?z)
      )


<a id="orgca9318a"></a>

## Create intermediate directories while saving files

Source: <https://superuser.com/questions/131538/can-i-create-directories-that-dont-exist-while-creating-a-new-file-in-emacs>

    (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
      "Create parent directory if not exists while visiting file."
      (unless (file-exists-p filename)
        (let ((dir (file-name-directory filename)))
          (unless (file-exists-p dir)
            (make-directory dir)))))


<a id="org7a347c0"></a>

## Shorcuts and registers


<a id="org4ea25da"></a>

### Registers

    (set-register ?n (cons 'file "~/my_org/notes.org"))
    (set-register ?l (cons 'file "~/application_letters/letter.md"))
    (set-register ?k (cons 'file "~/application_letters/Cover_letter_Shreyas_R.pdf"))
    (set-register ?p (cons 'file "~/org_cv/CV_Shreyas_Ragavan.pdf"))
    (set-register ?r (cons 'file "~/org_cv/CV_Shreyas_Ragavan.org"))
    (set-register ?t (cons 'file "~/my_org/todo-global.org"))
    (set-register ?i (cons 'file "~/dotemacs/.emacs.d/new-init.org"))
    (set-register ?j (cons 'file "~/my_org/mrps_canjs.org"))
    (set-register ?f (cons 'file "~/scimax/user/sr-cust/"))
    (set-register ?d (cons 'file "~/my_org/datascience.org"))
    (set-register ?m (cons 'file "~/my_org/"))
    (set-register ?g (cons 'file "~/my_gits/"))


<a id="orgf8b6144"></a>

### Google this

    (global-set-key (kbd "M-s g") 'google-this-mode-submap)


<a id="orge065622"></a>

### ivy-yasnippet

    (global-set-key (kbd "M-i") 'ivy-yasnippet)


<a id="orgc8d7829"></a>

### Mu4e related

    (global-set-key (kbd "M-s u") 'mu4e-update-mail-and-index)
    (global-set-key (kbd "M-s m") 'mu4e~headers-jump-to-maildir)
    (global-set-key (kbd "C-x m") 'mu4e-compose-new)


<a id="org85e1734"></a>

### Org related

    (global-set-key (kbd "C-x t") 'org-insert-todo-heading)
    (global-set-key (kbd "C-c d") 'org-time-stamp)
    (global-set-key (kbd "M-s s") 'org-save-all-org-buffers)
    (global-set-key (kbd "M-s j") 'org-journal-new-entry)


<a id="org70eb956"></a>

## yanking links in org format

Source: sacha chua.

Enables inserting a URL into an org document as '[<URL>][link]' by tapping F6 after copying the URL. This is useful to reduce clutter with long links, and even include links in headings.

    (defun my/yank-more ()
      (interactive)
      (insert "[[")
      (yank)
      (insert "][link]]"))
    (global-set-key (kbd "<f6>") 'my/yank-more)


<a id="orgce6f47d"></a>

## Export setup

    (require 'ox-org)
    (require 'ox-word)
    (require 'ox-md)


<a id="org11e865e"></a>

## Markdown config

Setting pandoc as the markdown command for live previews. The default command is `markdown`, which could be installed as a separate package.

    (setq markdown-command "pandoc")


<a id="org4f84f7d"></a>

## TEST Export async

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-14 Thu 16:03] </span></span>
    This requires a separate init file to be setup that enables Emacs to launch a separate process to export large files. It would be better as a vanilla emacs file.

    (setq org-export-async-init-file
          (expand-file-name "async-export.el" user-emacs-directory)
          )


<a id="orgdf06486"></a>

## TEST Ob-async

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-14 Thu 16:02] </span></span>
    This should enable evaluating code in org babel source blocks asynchronously. The header in the source block should have the async enabled.

    (use-package ob-async
      :ensure t
      )


<a id="orga5f77c3"></a>

## Completed loading message

    (message "Loaded Emacs general config")


<a id="orga962dce"></a>

# TODO Crux  - basic movement

Source: <https://jamiecollinson.com/blog/my-emacs-config/>
Contains functions from  Prelude. I should check this out in more detail.

Set C-a to move to the first non-whitespace character on a line, and then to toggle between that and the beginning of the line.

    (use-package crux
      :ensure t
      :bind (("C-a" . crux-move-beginning-of-line)))


<a id="orgbfed7f0"></a>

# Dired

Source: <https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org>

    (use-package dired
      :ensure nil
      :delight dired-mode "Dired"
      :preface
      (defun me/dired-directories-first ()
        "Sort dired listings with directories first before adding marks."
        (save-excursion
          (let (buffer-read-only)
            (forward-line 2)
            (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
          (set-buffer-modified-p nil)))
      ;:hook ;(dired-mode . dired-hide-details-mode)
      :config
      (advice-add 'dired-readin :after #'me/dired-directories-first)
      (setq-default
       dired-auto-revert-buffer t
       dired-dwim-target t
       dired-hide-details-hide-symlink-targets nil
       dired-listing-switches "-alh"
       dired-ls-F-marks-symlinks nil
       dired-recursive-copies 'always))

    (use-package dired-x
      :ensure nil
      :preface
      (defun me/dired-revert-after-command (command &optional output error)
        (revert-buffer))
      :config
      (advice-add 'dired-smart-shell-command :after #'me/dired-revert-after-command))

    (message "Loaded Dired customisation")


<a id="orgfacf8c1"></a>

# Swiper

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 16:50] </span></span>
    I use swiper for a general search. However [helm-swoop](#orgf56ffc0) is awesome.

    (global-set-key (kbd "C-s") 'swiper)
    (setq ivy-display-style 'fancy)

    ;; advise swiper to recenter on exit
    (defun bjm-swiper-recenter (&rest args)
      "recenter display after swiper"
      (recenter)
      )
    (advice-add 'swiper :after #'bjm-swiper-recenter)

    (message "Loaded Swiper customisation")


<a id="org4ba2d61"></a>

# Easier selection


<a id="orge0d780d"></a>

## TODO Expand region

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:27]  </span></span>
    Explore how this works

    (use-package expand-region
      :ensure t
      :bind ("C-=" . er/expand-region))

    (message "Loaded easier selection")


<a id="orga45c69a"></a>

# git related


<a id="orgaca43d2"></a>

## TODO Git gutter

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:30]  </span></span>
    Started using this today. It is actually very convenient to quickly view the changes made in the document. There is a function to pop up the changes at that location. I need to learn more about using this tool effectively.

    (use-package git-gutter
      :ensure t
      :config
      (global-git-gutter-mode 't)
      :diminish git-gutter-mode)


<a id="org13a0da1"></a>

## magit settings

    (setq magit-revert-buffers 'silent)


<a id="org52cdc3f"></a>

## TODO Time machine for git

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-08 Fri 13:21] </span></span>
    Launched by `M-x git-timemachine`, this lets you navigate through the commit history with a single key press! This is especially awesome for tracking changes to a particular snippet of code.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:30]  </span></span>
    Need to evaluate this. The purpose is for stepping through the history of a file recorded in git. This should be very interesting.

    (use-package git-timemachine
      :ensure t)


<a id="org923aeb6"></a>

## Completed loading message

    (message "Loaded git related config")


<a id="orgac6c04c"></a>

# Writeroom customisations

The goal is to enable a customised zen writing mode, especially facilitating blog posts and other longer forms of writing. As of now, there are customisations for the width, and calling the art-bollocks mode when writeroom mode is enabled.

    (with-eval-after-load 'writeroom-mode
      (define-key writeroom-mode-map (kbd "C-s-,") #'writeroom-decrease-width)
      (define-key writeroom-mode-map (kbd "C-s-.") #'writeroom-increase-width)
      (define-key writeroom-mode-map (kbd "C-s-=") #'writeroom-adjust-width))

    (advice-add 'text-scale-adjust :after
    	    #'visual-fill-column-adjust)

    ;;  loading artbollocks whenever the writeroom mode is called in particular.
    (autoload 'artbollocks-mode "artbollocks-mode")
    (add-hook 'writeroom-mode-hook 'artbollocks-mode)

    (message "Loaded writeroom customisations")


<a id="orgea18070"></a>

# ESS configuration <code>[/]</code>

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-09 Sat 12:36] </span></span>
    Set this up with use-package and explore further customisations. As of now, I use yasnippet to insert commonly used operators like the assign and pipe operators.

    ;; Setting up emacs ess and polymode
    (require 'ess)
    (require 'ess-R-data-view)
    (setq ess-describe-at-point-method 'tooltip)
    (setq ess-switch-to-end-of-proc-buffer t)
    (require 'ess-rutils)
    (setq ess-rutils-keys +1)
    (setq ess-eval-visibly 'nowait)
    (setq ess-use-flymake nil)
    (setq ess-use-eldoc t)

    (use-package ess-view
      :ensure t
      :config
      (if (system-type-is-darwin)
          (setq ess-view--spreadsheet-program "/Applications/Tad.app/Contents/MacOS/Tad")
        )
      )

    (message "Loaded ESS configuration")


<a id="org4d5908f"></a>

## TODO Set condition and path for TAD on Linux


<a id="orgd9ec89c"></a>

# TODO lintr

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-11 Mon 07:21] </span></span>
    It appears there is no package called lintr. This needs further investigation.

This package is deemed necessary to enable flymake in ESS. Without it, there is significantly more lag while the suggestions / corrections are generated in ESS modes.

    (use-package lintr
      :ensure nil
    )


<a id="org6b602a5"></a>

# Polymode

    (require 'poly-markdown)
    (require 'poly-R)

    ;; MARKDOWN
    (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))


    ;; R modes
    (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
    (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

    (message "Loaded polymode configuration")


<a id="org1530dcc"></a>

# Multiple Cursors

    (use-package multiple-cursors
      :ensure t
      :config
      (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
      )

    (message "Loaded MC")


<a id="org61b8ef7"></a>

# ox-reveal - presentations

    (use-package ox-reveal
      :ensure ox-reveal
      :defer t
      :config
      (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
      (setq org-reveal-mathjax t)
      )

    (use-package htmlize
      :ensure t)

    (message "Loaded ox-reveal cust")


<a id="org1b9feb9"></a>

# Org-mode related


<a id="org51f9c5e"></a>

## Default org directory and setting it as the agenda file directory

    (setq
     org-directory "~/my_org/"
     org-agenda-files '("~/my_org/")
     )


<a id="orga4f4f81"></a>

## Org-notes into log drawer

I've been inserting org notes into the body of the text, since I do not make extensive use of the log book in the agenda and prefer active time stamped notes and the org-journal and org-projectile to take down 'linked' log notes. However, I would like the notes to be inserted after any properties drawers.

    (setq org-log-state-notes-insert-after-drawers t)
    (setq org-log-redeadline 'time)


<a id="org9e1e554"></a>

## TODO Enabling org capture and org protocol

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:55]  </span></span>
    Need to actually get org-capture via external browser protocol working. Not sure if I need to require org-capture in scimax.

Source: <http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/>

    (require 'org-capture)
    (require 'org-protocol)


<a id="orgef7a4a9"></a>

## TODO Ensuring archive files are also in org mode

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:31]  </span></span>
    check whether the add-to-list function is sufficient.

    (add-hook 'find-file-hooks
              (lambda ()
                (let ((file (buffer-file-name)))
                  (when (and file (equal (file-name-directory file) "~/my_org/archive/"))
                    (org-mode)))))

    (add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))


<a id="org65c8413"></a>

## Archiving mechanics

Archive organised by Top level headings in the original file and with Tag preservation


    (defun my-org-inherited-no-file-tags ()
      (let ((tags (org-entry-get nil "ALLTAGS" 'selective))
            (ltags (org-entry-get nil "TAGS")))
        (mapc (lambda (tag)
                (setq tags
                      (replace-regexp-in-string (concat tag ":") "" tags)))
              (append org-file-tags (when ltags (split-string ltags ":" t))))
        (if (string= ":" tags) nil tags)))

    (defadvice org-archive-subtree (around my-org-archive-subtree-low-level activate)
      (let ((tags (my-org-inherited-no-file-tags))
            (org-archive-location
             (if (save-excursion (org-back-to-heading)
                                 (> (org-outline-level) 1))
                 (concat (car (split-string org-archive-location "::"))
                         "::* "
                         (car (org-get-outline-path)))
               org-archive-location)))
        ad-do-it
        (with-current-buffer (find-file-noselect (org-extract-archive-file))
          (save-excursion
            (while (org-up-heading-safe))
            (org-set-tags-to tags)))))


<a id="org1dd12bc"></a>

## Org journal

    (use-package org-journal
      :ensure t
      :defer t
      :custom
      (org-journal-dir "~/my_org/journal/")
      (org-journal-file-format "%Y%m%d")
      (org-journal-enable-agenda-integration t)
      (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode))
      ;; (org-journal-date-format "%A, %d %B %Y")
      ;; (org-journal-enable-encryption 't)
      )


<a id="org6162530"></a>

### TODO Figure out easy encryption approach for org journal


<a id="orgdd7a8c6"></a>

## Use Org ID for storing objects

Using the org id for reference to headings ensures that even if the heading changes, the links will still work.

    (setq org-id-method (quote uuidgen))


<a id="org15ee8d0"></a>

## TODO Setting custom keywords with fast access

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 12:19] </span></span>
    This requires a complete reload of org to come in effect.

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "CANCEL(c)" "POSTPONED(p)" "|" "DONE(d)" "STABLE(s)")
            (sequence "TEST(T)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
            (sequence "|" )))


<a id="org7cae1c4"></a>

## Cosmetics for org


<a id="org88cfecd"></a>

### TODO Basic cosmetics. Review & Convert to use-package style

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:20]  </span></span>
    These settings have to be cleaned up and the code optimised.

    (setq org-hide-leading-stars t)
    ;;(setq org-alphabetical-lists t)
    (setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
    (setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
    (setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
    (setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

    ;; Highlighting lines in the agenda, where the cursor is placed.
    (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

    ;; Setting up clean indenting below respective headlines at startup. - from the org mode website
    (setq org-startup-indented t)

    ;; use org bullets from emacsist
    (use-package org-bullets
      :ensure t
      :init
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


<a id="org51e1cd9"></a>

### Setting font faces for headline level

    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
     )


<a id="orgc683e02"></a>

### Striking out Done headlines

source: Sacha Chua

    (setq org-fontify-done-headline t)
    (custom-set-faces
     '(org-done ((t (:foreground "PaleGreen"
    			     :weight normal
    			     :strike-through t))))
     '(org-headline-done
       ((((class color) (min-colors 16) (background dark))
         (:foreground "LightSalmon" :strike-through t)))))


<a id="org6c58420"></a>

### Formatting keywords as boxes with inverted colors

Source : SO [link](https://stackoverflow.com/questions/12707492/add-custom-markers-to-emacs-org-mode) ,

    (set-face-attribute 'org-todo nil
                        :box '(:line-width 2
                               :color "black"
                               :style released-button)
                        :inverse-video t
                        )
    (set-face-attribute 'org-done nil
                        :box '(:line-width 2
                               :color "black"
                               :style released-button)
                        :inverse-video t
                        )
    (set-face-attribute 'org-priority nil
                        :inherit font-lock-keyword-face
                        :inverse-video t
                        :box '(:line-width 2
                               :color "black"
                               :style released-button)
                        )


<a id="org5a481c4"></a>

## Refiling settings


<a id="org8dae0b9"></a>

### Refile target level for search

    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))


<a id="orgb943829"></a>

### TODO General refile settings

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:33]  </span></span>
    Needs further review and optimisation

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-reverse-note-order t)
    (setq org-refile-allow-creating-parent-nodes 'confirm)


<a id="org86733e9"></a>

### [Also refer Refiling hydra](#orgac86ce6)


<a id="orgfe80a43"></a>

## Agenda mechanics


<a id="org1d4223c"></a>

### Weekday starts on Monday

    (setq org-agenda-start-on-weekday 1)


<a id="org7b277ca"></a>

### Display heading tags farther to the right

    (setq org-agenda-tags-column -150)


<a id="org731c5bd"></a>

### TODO Agenda customisation

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:26]  </span></span>
    Need to clear up the search functions, enabling complete search in journal files. Archive and some external directories are included, since they are explictly in org mode.


    (setq org-agenda-custom-commands
          '(("c" "Simple agenda view"
             ((tags "recurr"
    		((org-agenda-overriding-header "Recurring Tasks")))
              (agenda "")
              (todo "")))
            ("o" agenda "Office mode" ((org-agenda-tag-filter-preset '("-course" "-habit" "-someday" "-book" "-emacs"))))
            ("qc" tags "+commandment")
    	("e" tags "+org")
    	("w" agenda "Today" ((org-agenda-tag-filter-preset '("+work"))))
    	("W" todo-tree "WAITING")
    	("q" . "Custom queries") ;; gives label to "q"
    	("d" . "ds related")	 ;; gives label to "d"
    	("ds" agenda "Datascience" ((org-agenda-tag-filter-preset '("+datascience"))))
    	("qw" agenda "MRPS" ((org-agenda-tag-filter-preset '("+canjs"))))
    	("qa" "Archive tags search" org-tags-view ""
             ((org-agenda-files (file-expand-wildcards "~/my_org/*.org*"))))
            ("j" "Journal Search" search ""
             ''((org-agenda-text-search-extra-files (file-expand-wildcards "~/my_org/journal/"))))
            ("S" search ""
    	 ((org-agenda-files '("~/my_org/"))
    	  (org-agenda-text-search-extra-files )))
    	)
          )


<a id="orgb4307f8"></a>

### Expanding search locations

    (setq org-agenda-text-search-extra-files (apply 'append
    						(mapcar
    						 (lambda (directory)
    						   (directory-files-recursively
    						    directory org-agenda-file-regexp))
    						 '("~/my_org/journal/" "~/my_org/zeeco_archive/" "~/my_projects/" ))))

1.  TODO Adding org archive for text search. Optimise this

    :CREATED:  <span class="timestamp-wrapper"><span class="timestamp">&lt;2019-02-07 Thu 08:29&gt;</span></span>

        (setq org-agenda-text-search-extra-files '(agenda-archives))


<a id="org8214bc9"></a>

### Enable default fuzzy search like in google

    (setq org-agenda-search-view-always-boolean t)


<a id="org8cdb53e"></a>

### Enable sticky agenda

Experimenting with this setting.

    (setq org-agenda-sticky t)


<a id="org583c5b1"></a>

### DONE org-habit

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 13:20] </span></span>
    Adding a require has brought org-habit back on track.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:50] </span></span>
    Appears the use-package config for org-habit is not correct and there is some issue in downloading it as a package.

I want to shift the org habit graph in the agenda further out right so as to leave enough room for the headings to be visible.

    (require 'org-habit)
    (setq org-habit-graph-column 90)


<a id="orgaf205c1"></a>

## TODO Capture mechanics

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:24]  </span></span>
    need to clean this up.


<a id="orgf6c1d2c"></a>

### Capture templates

    (setq org-capture-templates
          '(("t" "Task entry")
            ("tt" "Todo - Fast" entry (file+headline "~/my_org/todo-global.org" "--Inbox")
    	 "** TODO %?")
            ("tb" "Todo -BGR" entry (file+headline "~/my_org/bgr.org" "#BGR #Inbox")
    	 "** TODO %?")
            ("te" "Todo - Emacs" entry (file+headline "~/my_org/todo-global.org" ";Emacs stuff")
    	 "** TODO %?")
    	("tm" "Mail Link Todo" entry (file+headline "~/my_org/todo-global.org" "--Inbox")
    	 "** TODO Mail: %a ")
            ("l" "Link/Snippet" entry (file+headline "~/my_org/link_database.org" ".UL Unfiled Links")
             "** %? %a ")
            ("e" "Protocol info" entry ;; 'w' for 'org-protocol'
             (file+headline "~/my_org/link_database.org" ".UL Unfiled Links")
             "*** %a, %T\n %:initial")
            ("n" "Notes")
            ("ne" "Emacs note" entry (file+headline "~/my_org/todo-global.org" ";Emacs stuff")
             "** %?")
            ("nn" "General note" entry (file+headline "~/my_org/notes.org" ".NOTES")
             "** %?")
            ("n" "Note" entry (file+headline "~/my_org/notes.org" ".NOTES")
             "** %?")
            ("b" "BGR stuff")
            ("bi" "Inventory project")
            ("bil" "Daily log" entry (file+olp+datetree "~/my_org/bgr.org" "Inventory management Project") "** %? %i")
            ("C" "Commandment" entry (file+datetree "~/my_org/lifebook.org" "")
             "** %? %i %T :commandment:")
            ("c" "canjs" entry (file+headline "~/my_org/mrps_canjs.org" "MRPS #CANJS")
             "** TODO %? %i %T")
            ("r" "Self Reflection" entry (file+datetree "~/my_org/lifebook.org" "")
             "b** %? %i %T :self_reflection:")
            ("w" "Website" plain
             (function org-website-clipper)
             "* %a %T\n" :immediate-finish t)
            ("j" "Journal Note"  plain (function get-journal-file-today) "* Event: %?\n\n  %i\n\n  " :empty-lines 1)
            ("i" "Whole article capture" entry
             (file+headline "~/my_org/full_article_archive.org" "" :empty-lines 1)
             "** %a, %T\n %:initial" :empty-lines 1)
            ("d" "Datascience stuff")
            ("dt" "Datascience inbox" entry (file+headline "~/my_org/datascience.org" "@Datascience @Inbox")
             "** TODO %? %T")
            ("dn" "Datascience note" entry (file+headline "~/my_org/datascience.org" "@Datascience @Notes")
             "** %? %T")
            ))


<a id="org641a0d8"></a>

### TODO Hooks for capture frame control

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:53]  </span></span>
    Needs further review.

Source: <http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection>

    (defadvice org-capture
        (after make-full-window-frame activate)
      "Advise capture to be the only window when used as a popup"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-other-windows)))

    (defadvice org-capture-finalize
        (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-frame)))


<a id="org9e5df58"></a>

## TODO version control and backup of files

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:15]  </span></span>
    Need to check out how this works and whether this is still necessary, since I am using Git.

    (setq delete-old-versions -1)
    (setq version-control t)


<a id="org6eba7a5"></a>

## Include gpg files in agenda generation

Source: <https://emacs.stackexchange.com/questions/36542/include-org-gpg-files-in-org-agenda>

    (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
      (setq org-agenda-file-regexp
            (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                      org-agenda-file-regexp)))


<a id="org24fe593"></a>

## org-noter

> Org-noter’s purpose is to let you create notes that are kept in sync when you scroll through the document, but that are external to it - the notes themselves live in an Org-mode file. As such, this leverages the power of Org-mode (the notes may have outlines, latex fragments, babel, etc…) while acting like notes that are made inside the document. Also, taking notes is very simple: just press i and annotate away!
>
> [Gonçalo Santos](https://github.com/weirdNox)

    (use-package org-noter
      :ensure t
      :defer t
      :config
      (setq org-noter-set-auto-save-last-location t)
      )


<a id="org1cc9bb9"></a>

## TODO Persp-projectile

[Refer Howard's config snippet](https://github.com/howardabrams/dot-files/blob/master/emacs.org#perspective) to setup a test.


<a id="org8eb6349"></a>

## TODO org-projectile

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:42]  </span></span>
    need to optimise further and convert to use-package style. Also need a way to capture Notes from projects, in addition to tasks.

Starting off with the basic configuration posted in org-projectile github repo.

    (require 'org-projectile)

    (setq org-projectile-projects-file
          "~/my_org/project-tasks.org")
    (push (org-projectile-project-todo-entry) org-capture-templates)

    ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    ;; Excluding the above since the entire my_org directory is already included in the agenda

    (global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)


<a id="org558a06b"></a>

## TODO org-gcal customisation


<a id="org7a67285"></a>

## TODO Property customisation


<a id="org71e3af3"></a>

### TODO Add a CREATED property for org heading

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:10]  </span></span>
    Needs further review and optimisation.


    (require 'org-expiry)
    ;; Configure it a bit to my liking
    (setq
     org-expiry-created-property-name "CREATED" ; Name of property when an item is created
     org-expiry-inactive-timestamps   nil         ; Don't have everything in the agenda view
     )

    (defun mrb/insert-created-timestamp()
      "Insert a CREATED property using org-expiry.el for TODO entries"
      (org-expiry-insert-created)
      (org-back-to-heading)
      (org-end-of-line)
      (insert " ")
      )

    ;; Whenever a TODO entry is created, I want a timestamp
    ;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
    (defadvice org-insert-todo-heading (after mrb/created-timestamp-advice activate)
      "Insert a CREATED property using org-expiry.el for TODO entries"
      (mrb/insert-created-timestamp)
      )
    ;; Make it active
    (ad-activate 'org-insert-todo-heading)

    (require 'org-capture)

    (defadvice org-capture (after mrb/created-timestamp-advice activate)
      "Insert a CREATED property using org-expiry.el for TODO entries"
       					; Test if the captured entry is a TODO, if so insert the created
       					; timestamp property, otherwise ignore
      (mrb/insert-created-timestamp))
    ;;  (when (member (org-get-todo-state) org-todo-keywords-1)
    ;;    (mrb/insert-created-timestamp)))
      (ad-activate 'org-capture)


<a id="orgbb536df"></a>

### Enabling adding tags in the capture window

    ;; Add feature to allow easy adding of tags in a capture window
    (defun mrb/add-tags-in-capture()
      (interactive)
      "Insert tags in a capture window without losing the point"
      (save-excursion
        (org-back-to-heading)
        (org-set-tags)))
    ;; Bind this to a reasonable key
    (define-key org-capture-mode-map "\C-c\C-t" 'mrb/add-tags-in-capture)


<a id="org6567e1f"></a>

## TODO org web clipper

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:11]  </span></span>
    This works fine now. However, it would be nice to find a way to strip the headers and menu columns and other unnecessary information before capture.

Source: <http://www.bobnewell.net/publish/35years/webclipper.html>

    ;; org-eww and org-w3m should be in your org distribution, but see
    ;; note below on patch level of org-eww.
    (require 'org-eww)
    (require 'org-w3m)
    (defvar org-website-page-archive-file "~/my_org/full_article_archive.org")
    (defun org-website-clipper ()
      "When capturing a website page, go to the right place in capture file,
       but do sneaky things. Because it's a w3m or eww page, we go
       ahead and insert the fixed-up page content, as I don't see a
       good way to do that from an org-capture template alone. Requires
       Emacs 25 and the 2017-02-12 or later patched version of org-eww.el."
      (interactive)

      ;; Check for acceptable major mode (w3m or eww) and set up a couple of
      ;; browser specific values. Error if unknown mode.

      (cond
       ((eq major-mode 'w3m-mode)
        (org-w3m-copy-for-org-mode))
       ((eq major-mode 'eww-mode)
        (org-eww-copy-for-org-mode))
       (t
        (error "Not valid -- must be in w3m or eww mode")))

      ;; Check if we have a full path to the archive file.
      ;; Create any missing directories.

      (unless (file-exists-p org-website-page-archive-file)
        (let ((dir (file-name-directory org-website-page-archive-file)))
          (unless (file-exists-p dir)
            (make-directory dir))))

      ;; Open the archive file and yank in the content.
      ;; Headers are fixed up later by org-capture.

      (find-file org-website-page-archive-file)
      (goto-char (point-max))
      ;; Leave a blank line for org-capture to fill in
      ;; with a timestamp, URL, etc.
      (insert "\n\n")
      ;; Insert the web content but keep our place.
      (save-excursion (yank))
      ;; Don't keep the page info on the kill ring.
      ;; Also fix the yank pointer.
      (setq kill-ring (cdr kill-ring))
      (setq kill-ring-yank-pointer kill-ring)
      ;; Final repositioning.
      (forward-line -1)
      )


<a id="orge944bb1"></a>

## Org-babel


<a id="orgf4a1faf"></a>

### Loading language base

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (scheme . t))
     )


<a id="org50d0989"></a>

### Clojure and cider

    (require 'cider)
    (setq org-babel-clojure-backend 'cider)


<a id="org45e8402"></a>

## TODO Org-trello


<a id="orgb9f859c"></a>

## Loading completed

    (message "Loaded org customisations")


<a id="org4c7c85f"></a>

# Helm


<a id="org45e8ffe"></a>

## Setting Helm to be used for specific functions

I prefer using Helm for specific functions like M-x, find files and bookmarks and switching buffers.

    ;; Setting Helm as preferred package to use
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)
    (global-set-key (kbd "C-x b") #'helm-mini)


<a id="org29ed532"></a>

## TODO Persistent follow mode for Helm

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 07:46]  </span></span>
    Need to find exactly what this does

    (custom-set-variables
     '(helm-follow-mode-persistent t))


<a id="orgca96cbd"></a>

## TODO Setting sources for helm

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 14:55] </span></span>
    This is still causing issues: the recentf list has to be cleared via helm-mini first.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 16:28] </span></span>
    This was needed as it seems helm was not sourcing from recentf file lists. With this source list defined, it provides options to choose from recent files, bookmarks, open buffers.

    (setq helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-bookmarks
                                      helm-source-bookmark-set
                                      helm-source-buffer-not-found))

    (setq helm-buffers-list-default-sources '(helm-source-buffers-list
                                              helm-source-recentf
                                              helm-source-bookmarks
                                              helm-source-bookmark-set
                                              helm-source-buffer-not-found))


<a id="orge65ce5c"></a>

## helm-ag and helm-org-rifle

    (require 'helm-ag)
    (require 'helm-org-rifle)


<a id="orgf56ffc0"></a>

## helm-swoop

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 16:53] </span></span>
    This is an awesome find. Helm swoop changes the search pattern depending on the location of the cursor. Therefore, while placed on an org headline, calling helm-swoop will preset the search pattern to have headings. The same is true for source code blocks! Fantastic.

Source: <https://writequit.org/org/#orgheadline92>

    (use-package helm-swoop
      :ensure t
      :bind (("M-i" . helm-swoop)
             ("M-I" . helm-swoop-back-to-last-point)
             ("C-c M-i" . helm-multi-swoop))
      :config
      ;; When doing isearch, hand the word over to helm-swoop
      (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
      ;; From helm-swoop to helm-multi-swoop-all
      (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t
            ;; If this value is t, split window inside the current window
            helm-swoop-split-with-multiple-windows t
            ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
            helm-swoop-split-direction 'split-window-vertically
            ;; If nil, you can slightly boost invoke speed in exchange for text color
            helm-swoop-speed-or-color nil))


<a id="org0d0be7d"></a>

## Loading completed

    (message "Loaded Helm customisations")


<a id="org945af6f"></a>

# Flycheck

Source: <https://writequit.org/org/>


<a id="org18446fe"></a>

## Basic config

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-09 Sat 11:51] </span></span>
    disabling flycheck for the moment and enabling flymake

    (use-package flycheck
      :defer 5
      :bind (("M-g M-n" . flycheck-next-error)
             ("M-g M-p" . flycheck-previous-error)
             ("M-g M-=" . flycheck-list-errors))
      :init (global-flycheck-mode)
      :diminish flycheck-mode
      :config
      (progn
        (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc json-jsonlint json-python-json ess iess))
        (use-package flycheck-pos-tip
          :init (flycheck-pos-tip-mode))
        (use-package helm-flycheck
          :init (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
        (use-package flycheck-haskell
          :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))))


<a id="org3f30965"></a>

## Replacing flycheck with flymake

This is especially for python modules at the moment.

    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))


<a id="org49e7bf1"></a>

# Scheme setup

-   References
    -   <http://praveen.kumar.in/2011/03/06/gnu-emacs-and-mit-scheme-on-mac-os-x/>

    (setq scheme-program-name "/Applications/MIT-GNU-Scheme.app/Contents/Resources/mit-scheme")
    (require 'xscheme)

    (message "Loaded scheme setup")


<a id="org577ba8d"></a>

# Hydras and some custom functions


<a id="orgac86ce6"></a>

## Refiling

Adapted from <https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location>

source: <https://gist.github.com/mm--/60e0790bcbf8447160cc87a66dc949ab>


    (defun my/refile (file headline &optional arg)
      "Refile to a specific location.

    With a 'C-u' ARG argument, we jump to that location (see
    `org-refile').

    Use `org-agenda-refile' in `org-agenda' mode."
      (let* ((pos (with-current-buffer (or (get-buffer file) ;Is the file open in a buffer already?
    				       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
    		(or (org-find-exact-headline-in-buffer headline)
    		    (error "Can't find headline `%s'" headline))))
    	 (filepath (buffer-file-name (marker-buffer pos))) ;If we're given a relative name, find absolute path
    	 (rfloc (list headline filepath nil pos)))
        (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
    	(org-agenda-refile nil rfloc)
          (org-refile arg nil rfloc))))

    (defun josh/refile (file headline &optional arg)
      "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.

    With a `C-u` ARG, just jump to the headline."
      (interactive "P")
      (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
        (cond
         ((and arg (listp arg))	    ;Are we jumping?
          (my/refile file headline arg))
         ;; Are we in org-capture-mode?
         (is-capturing      	;Minor mode variable that's defined when capturing
          (josh/org-capture-refile-but-with-args file headline arg))
         (t
          (my/refile file headline arg)))
        (when (or arg is-capturing)
          (setq hydra-deactivate t))))

    (defun josh/org-capture-refile-but-with-args (file headline &optional arg)
      "Copied from `org-capture-refile' since it doesn't allow passing arguments. This does."
      (unless (eq (org-capture-get :type 'local) 'entry)
        (error
         "Refiling from a capture buffer makes only sense for `entry'-type templates"))
      (let ((pos (point))
    	(base (buffer-base-buffer (current-buffer)))
    	(org-capture-is-refiling t)
    	(kill-buffer (org-capture-get :kill-buffer 'local)))
        (org-capture-put :kill-buffer nil)
        (org-capture-finalize)
        (save-window-excursion
          (with-current-buffer (or base (current-buffer))
    	(org-with-wide-buffer
    	 (goto-char pos)
    	 (my/refile file headline arg))))
        (when kill-buffer (kill-buffer base))))

    (defmacro josh/make-org-refile-hydra (hydraname file keyandheadline)
      "Make a hydra named HYDRANAME with refile targets to FILE.
    KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
      `(defhydra ,hydraname (:color blue :after-exit (unless (or hydra-deactivate
    							     current-prefix-arg) ;If we're just jumping to a location, quit the hydra
    						   (josh/org-refile-hydra/body)))
         ,file
         ,@(cl-loop for kv in keyandheadline
    		collect (list (car kv) (list 'josh/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
         ("q" nil "cancel")))

    ;;;;;;;;;;
    ;; Here we'll define our refile headlines
    ;;;;;;;;;;

    (josh/make-org-refile-hydra josh/org-refile-hydra-file-ds
    			    "~/my_org/datascience.org"
    			    (("1" . "@Datascience @Inbox")
    			     ("2" . "@Datascience @Notes")))

    (josh/make-org-refile-hydra josh/org-refile-hydra-file-bgr
    			    "~/my_org/bgr.org"
    			    (("1" . "#BGR #Inbox")
    			     ("2" . "#questions @ BGR")
                                 ("3" . "Inventory management Project")))

    (josh/make-org-refile-hydra josh/org-refile-hydra-file-todoglobal
    			    "todo-global.org"
    			    (("1" . ";Emacs Stuff")
    			     ("2" . ";someday")))

    (defhydra josh/org-refile-hydra (:foreign-keys run)
      "Refile"
      ("a" josh/org-refile-hydra-file-ds/body "File A" :exit t)
      ("b" josh/org-refile-hydra-file-bgr/body "File B" :exit t)
      ("c" josh/org-refile-hydra-file-todoglobal/body "File C" :exit t)
      ("j" org-refile-goto-last-stored "Jump to last refile" :exit t)
      ("q" nil "cancel"))

    (global-set-key (kbd "<f8> r") 'josh/org-refile-hydra/body)


<a id="org669d38a"></a>

## Window manipulation

Source : Hydra documentation


    ;;  Hydras for window configuration. Using the deluxe
    (defhydra hydra-window ()
      "
    Movement^^        ^Split^         ^Switch^		^Resize^
    ----------------------------------------------------------------
    _h_ ←       	_v_ertical    	_b_uffer		_q_ X←
    _j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
    _k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
    _l_ →        	_Z_ reset      	_s_wap		_r_ X→
    _F_ollow		_D_lt Other   	_S_ave		max_i_mize
    _SPC_ cancel	_o_nly this   	_d_elete
    "
      ("h" windmove-left )
      ("j" windmove-down )
      ("k" windmove-up )
      ("l" windmove-right )
      ("q" hydra-move-splitter-left)
      ("w" hydra-move-splitter-down)
      ("e" hydra-move-splitter-up)
      ("r" hydra-move-splitter-right)
      ("b" helm-mini)
      ("f" helm-find-files)
      ("F" follow-mode)
      ("a" (lambda ()
             (interactive)
             (ace-window 1)
             (add-hook 'ace-window-end-once-hook
                       'hydra-window/body))
       )
      ("v" (lambda ()
             (interactive)
             (split-window-right)
             (windmove-right))
       )
      ("x" (lambda ()
             (interactive)
             (split-window-below)
             (windmove-down))
       )
      ("s" (lambda ()
             (interactive)
             (ace-window 4)
             (add-hook 'ace-window-end-once-hook
                       'hydra-window/body)))
      ("S" save-buffer)
      ("d" delete-window)
      ("D" (lambda ()
             (interactive)
             (ace-window 16)
             (add-hook 'ace-window-end-once-hook
                       'hydra-window/body))
       )
      ("o" delete-other-windows)
      ("i" ace-maximize-window)
      ("z" (progn
             (winner-undo)
             (setq this-command 'winner-undo))
       )
      ("Z" winner-redo)
      ("SPC" nil)
      )

    (global-set-key (kbd "<f8> w") 'hydra-window/body)


<a id="org14502d4"></a>

## helm-do-ag in specific locations

Reference: <https://emacs.stackexchange.com/questions/44128/function-to-do-helm-do-ag-for-a-specific-project>


<a id="org7b8bbeb"></a>

### In project directory

    (defun helm-do-ag-projects ()
      "Grep string in Project directory" (interactive)
      (let ((rootdir (concat "~/my_projects/")))
        (let ((helm-ag-command-option (concat helm-ag-command-option "")))
          (helm-do-ag rootdir))))


<a id="orgce9c3a7"></a>

### Scimax config directory

    (defun helm-do-ag-emacs-config ()
      "Grep string in Emacs custom code"
      (interactive)
      (let ((rootdir (concat "~/scimax/user/sr-cust/")))
        (let ((helm-ag-command-option (concat helm-ag-command-option "")))
          (helm-do-ag rootdir))))


<a id="orgbf2a6e0"></a>

### Journal directory

    (defun helm-do-ag-journal ()
      "Grep string in journal"
      (interactive)
      (let ((specfile (concat "~/my_org/journal/")))
        (let ((helm-ag-command-option (concat helm-ag-command-option "")))
          (helm-ag-this-file rootdir))))


<a id="org58d91fa"></a>

### BGR file

    (defun helm-do-ag-bgr ()
      "Grep string in BGR file"
      (interactive)
      (let ((specfile (concat "~/my_org/bgr.org")))
        (let ((helm-ag-command-option (concat helm-ag-command-option "")))
          (helm-do-ag-this-file specfile))))


<a id="orgf9318c6"></a>

### Defining hydra

    (defhydra shrysr/hydra-helm-ag-do-menu ()
      "
    Helm-do-ag in specified locations
    ^location^  ^command^
    ----------------------------------------------------------
    e:        emacs custom config
    b:        bgr file
    o:        org files
    j:        journal search
    "
      ("e" helm-do-ag-emacs-config)
      ("j" helm-do-ag-journal :color blue)
      ("p" helm-do-ag-projects)
      ("o" helm-do-ag-org)
      ("q" quit-window "quit" :color red))

    (global-set-key (kbd "<f8> h") 'shrysr/hydra-helm-ag-do-menu/body)


<a id="org1bc58a8"></a>

## Frame configurations fo magit and project launch


<a id="orgac3a010"></a>

### Scimax - magit and windows

    ;; scimax directory magit status
    (defun sr/windows-magit-scimax ()
      (interactive)
      (ace-delete-other-windows)
      (dired "~/scimax/user/")
      (switch-window-then-split-right nil)
      (magit-status "~/scimax/")
      (switch-window)
      (split-window-vertically)
      (dired-up-directory)
      (windmove-right)
      )


<a id="orgf7fe94c"></a>

### Org files - magit and windows

    ;; my_org magit status
    (defun sr/windows-magit-org ()
      (interactive)
      (ace-delete-other-windows)
      (magit-status "~/my_org/")
      )


<a id="org3d5ceb6"></a>

### Project directory - magit and windows

    ;; magit status
    (defun sr/windows-magit-projects ()
      (interactive)
      (ace-delete-other-windows)
      (switch-window-then-split-right nil)
      (magit-status "~/my_projects/")
      (switch-window)
      (dired "~/my_projects/")
      (switch-window)
      )


<a id="org8ccc4ae"></a>

### TODO Project: Switch and windows

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-10 Sun 07:09] </span></span>
    Experiment with helm-swoop functions to target only top level headings

    (defun sr/windows-projects ()
      (interactive)
      (ace-delete-other-windows)
      (switch-window-then-split-right nil)
      (projectile-switch-project)
      (switch-window)
      (find-file "~/my_org/project-tasks.org")
      (widen)
      (helm-org-rifle-current-buffer)
      (org-narrow-to-subtree)
      (outline-show-children)
      )


<a id="org4c7d57e"></a>

### Defining Hydra

    (defhydra sr/process-window-keys ()
      "
    Key^^   ^Workflow^
    --------------------
    o       org magit
    s       scimax magit
    p       projects magit
    w       select project and set window config
    SPC     exit
    "
      ("o" sr/windows-magit-org )
      ("p" sr/windows-magit-projects )
      ("s" sr/windows-magit-scimax )
      ("w" sr/windows-projects)
      ("SPC" nil)
      )

    (global-set-key (kbd "<f8> m") 'sr/process-window-keys/body)


<a id="org53523d1"></a>

## Loading completed

    (message "Loaded Hydras")


<a id="org378cec0"></a>

# Elfeed customisation


<a id="orgbd8d288"></a>

## Elfeed Basic + Customisations

Source: <http://heikkil.github.io/blog/2015/05/09/notes-from-elfeed-entries/>

    ;; Elfeed configuration source :
    (use-package elfeed
      :bind ("C-c f" . elfeed)
      :init
      (setq my/default-elfeed-search-filter "@1-month-ago +unread !sport ")
      (setq-default elfeed-search-filter my/default-elfeed-search-filter)
      (setq elfeed-db-direcory "~/scimax/user/elfeeddb")
      :config
      (elfeed-org)

      ;;
      ;; linking and capturing
      ;;

      (defun elfeed-link-title (entry)
        "Copy the entry title and URL as org link to the clipboard."
        (interactive)
        (let* ((link (elfeed-entry-link entry))
               (title (elfeed-entry-title entry))
               (titlelink (concat "[[" link "][" title "]]")))
          (when titlelink
            (kill-new titlelink)
            (x-set-selection 'PRIMARY titlelink)
            (message "Yanked: %s" titlelink))))

      ;; show mode

      (defun elfeed-show-link-title ()
        "Copy the current entry title and URL as org link to the clipboard."
        (interactive)
        (elfeed-link-title elfeed-show-entry))

      (defun elfeed-show-quick-url-note ()
        "Fastest way to capture entry link to org agenda from elfeed show mode"
        (interactive)
        (elfeed-link-title elfeed-show-entry)
        (org-capture nil "n")
        (yank)
        (org-capture-finalize))

      (bind-keys :map elfeed-show-mode-map
                 ("l" . elfeed-show-link-title)
                 ("v" . elfeed-show-quick-url-note))

      ;; search mode

      (defun elfeed-search-link-title ()
        "Copy the current entry title and URL as org link to the clipboard."
        (interactive)
        (let ((entries (elfeed-search-selected)))
          (cl-loop for entry in entries
                   when (elfeed-entry-link entry)
                   do (elfeed-link-title entry))))

      (defun elfeed-search-quick-url-note ()
        "In search mode, capture the title and link for the selected
         entry or entries in org aganda."
        (interactive)
        (let ((entries (elfeed-search-selected)))
          (cl-loop for entry in entries
                   do (elfeed-untag entry 'unread)
                   when (elfeed-entry-link entry)
                   do (elfeed-link-title entry)
                   do (org-capture nil "n")
                   do (yank)
                   do (org-capture-finalize)
                   (mapc #'elfeed-search-update-entry entries))
          (unless (use-region-p) (forward-line))))

      (bind-keys :map elfeed-search-mode-map
                 ("l" . elfeed-search-link-title)
                 ("v" . elfeed-search-quick-url-note)))


<a id="org1f48e7d"></a>

## Elfeed-org and elfeed-goodies setup <code>[/]</code>

Using an org source is the easiest way to organise my RSS feeds for reading with Elfeed.

    ;; use an org file to organise feeds
    (use-package elfeed-org
      :ensure t
      :config
      (setq rmh-elfeed-org-files (list "~/my_org/elfeed.org"))
      )

    (use-package elfeed-goodies
      :ensure t
      :init
      (elfeed-goodies/setup)
    )


<a id="orgebf7de8"></a>

## Functions to support syncing

    ;;functions to support syncing .elfeed between machines
    ;;makes sure elfeed reads index from disk before launching
    (defun bjm/elfeed-load-db-and-open ()
      "Wrapper to load the elfeed db from disk before opening"
      (interactive)
      (elfeed-db-load)
      (elfeed)
      (elfeed-search-update--force))

    ;;write to disk when quiting
    (defun bjm/elfeed-save-db-and-bury ()
      "Wrapper to save the elfeed db to disk before burying buffer"
      (interactive)
      (elfeed-db-save)
      (quit-window))

    (use-package elfeed
      :ensure t
      :bind (:map elfeed-search-mode-map
                  ("A" . bjm/elfeed-show-all)
                  ("E" . bjm/elfeed-show-emacs)
                  ("D" . bjm/elfeed-show-daily)
                  ("q" . bjm/elfeed-save-db-and-bury)))


<a id="org5f6495b"></a>

## TODO Consider storing the Feed sources here in org format

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-17 Sun 18:11] </span></span>
    This will need an export to a source org file per the settings.


<a id="org27083ab"></a>

## Loading completed

    (message "Loaded Elfeed customisations")


<a id="org6378b32"></a>

# w3m customisation

A few snippets were sourced from: <http://beatofthegeek.com/2014/02/my-setup-for-using-emacs-as-web-browser.html>

    (setq browse-url-browser-function 'w3m-goto-url-new-session)
    (setq w3m-default-display-inline-images t)


<a id="orge4ae54f"></a>

## TODO Appending HTTP to web addresses entered by hand

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 07:40]  </span></span>
    Check whether this is necessary

    ;;when I want to enter the web address all by hand
    (defun w3m-open-site (site)
      "Opens site in new w3m session with 'http://' appended"
      (interactive
       (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
      (w3m-goto-url-new-session
       (concat "http://" site)))


<a id="org1b90cfb"></a>

## Changing w3m shortcuts for better tabbed browsing

Source:  Sacha Chua : <http://sachachua.com/blog/2008/09/emacs-and-w3m-making-tabbed-browsing-easier/>

    (eval-after-load 'w3m
      '(progn
         (define-key w3m-mode-map "q" 'w3m-previous-buffer)
         (define-key w3m-mode-map "w" 'w3m-next-buffer)
         (define-key w3m-mode-map "x" 'w3m-close-window)))


<a id="org0ece42b"></a>

## TODO Default external browser settings

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 07:37]  </span></span>
    Need to have this change depending whether the OS is Linux or Mac OS

    (defun wicked/w3m-open-current-page-in-firefox ()
      "Open the current URL in Mozilla Firefox."
      (interactive)
      (browse-url-default-macosx-browser w3m-current-url)) ;; (1)

    (defun wicked/w3m-open-link-or-image-in-firefox ()
      "Open the current link or image in Firefox."
      (interactive)
      (browse-url-default-macosx-browser (or (w3m-anchor) ;; (2)
                                             (w3m-image)))) ;; (3)

    (eval-after-load 'w3m
      '(progn
         (define-key w3m-mode-map "o" 'wicked/w3m-open-current-page-in-firefox)
         (define-key w3m-mode-map "O" 'wicked/w3m-open-link-or-image-in-firefox)))


<a id="orga65ebe1"></a>

## Wikipedia search

    (defun wikipedia-search (search-term)
      "Search for SEARCH-TERM on wikipedia"
      (interactive
       (let ((term (if mark-active
                       (buffer-substring (region-beginning) (region-end))
                     (word-at-point))))
         (list
          (read-string
           (format "Wikipedia (%s):" term) nil nil term)))
       )
      (browse-url
       (concat
        "http://en.m.wikipedia.org/w/index.php?search="
        search-term
        ))
      )


<a id="org45e689c"></a>

## Access Hacker News

    (defun hn ()
      (interactive)
      (browse-url "http://news.ycombinator.com"))


<a id="orgf4e1456"></a>

# ediff

I have to diff between org files pretty often, and need the headings to be unfolded.

Source: <http://emacs.stackexchange.com/questions/21335/prevent-folding-org-files-opened-by-ediff>

    ;; Check for org mode and existence of buffer
    (defun f-ediff-org-showhide (buf command &rest cmdargs)
      "If buffer exists and is orgmode then execute command"
      (when buf
        (when (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
          (save-excursion (set-buffer buf) (apply command cmdargs)))))

    (defun f-ediff-org-unfold-tree-element ()
      "Unfold tree at diff location"
      (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
      (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
      (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

    (defun f-ediff-org-fold-tree ()
      "Fold tree back to top level"
      (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
      (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
      (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

    (add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
    (add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)


<a id="org3f0ebe6"></a>

# Theme and visuals


<a id="orgb222ff5"></a>

## Emacsclient or frame specific settings

Since I run emacs as a daemon and call the emacsclient, the background has to be set for new frames. Additionally, I'd like the frames to launch full screen.

    (setq default-frame-alist
          '((background-color . "light grey")
            (foreground-color . "black")
            (fullscreen . maximized)
            ))


<a id="orgbea9578"></a>

## Custom Safe themes and Background change to light grey

    (setq custom-safe-themes t)
    (set-background-color "light gray")


<a id="org31e27c6"></a>

## Font Customisation based on OS

The same font is named differently in Antergos (Linux) and in the Mac OS.

    ;; For Linux
    (if (system-type-is-gnu)
        (set-face-attribute 'default nil :family "ttf-iosevka" :height 140))

    ;; For Mac OS
    (if (system-type-is-darwin)
        (set-face-attribute 'default nil :family "Iosevka Type" :height 150))


<a id="org8797f53"></a>

## visual-fill-column

Source: <https://github.com/wasamasa/dotemacs/blob/master/init.org>

Adding a hook to enable visual fill column mode once visual line mode is called did not work for me. The issue is that this is called

    (setq
     global-visual-line-mode 1
     fill-column 80)


<a id="org587f7ce"></a>

# Hugo


<a id="org810c14e"></a>

## Function to create specific properties for a blog post

Modified this function from:


<a id="org587bf4b"></a>

### TODO Defining content directory

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 08:06]  </span></span>
    Need to check if this is still required since I have switche to ox-hugo

    (defvar hugo-content-dir "~/my_gits/hugo-sr/content/post/"
      "Path to Hugo's content directory")


<a id="orgbfc8562"></a>

### Ensuring properties exist and creating if they dont exist

    (defun hugo-ensure-property (property)
      "Make sure that a property exists. If not, it will be created.
    Returns the property name if the property has been created, otherwise nil."
      (org-id-get-create)
      (if (org-entry-get nil property)
          nil
        (progn (org-entry-put nil property "")
               property)))

    (defun hugo-ensure-properties ()

      (require 'dash)
      (let ((current-time (format-time-string
                           (org-time-stamp-format t t) (org-current-time)))
            first)
        (save-excursion
          (setq first (--first it (mapcar #'hugo-ensure-property
                                          '("HUGO_TAGS" "HUGO_CATEGORIES"))))
          (unless (org-entry-get nil "HUGO_DATE")
            (org-entry-put nil "EXPORT_DATE" current-time)))
        (org-entry-put nil "EXPORT_FILE_NAME" (org-id-get-create))
        (org-entry-put nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER" ":profile false")
        (when first
          (goto-char (org-entry-beginning-position))
          ;; The following opens the drawer
          (forward-line 1)
          (beginning-of-line 1)
          (when (looking-at org-drawer-regexp)
            (org-flag-drawer nil))
          ;; And now move to the drawer property
          (search-forward (concat ":" first ":"))
          (end-of-line))
        first))


<a id="orge5288d5"></a>

### Hugo function calling the above

    (defun hugo ()
      (interactive)
      (unless (hugo-ensure-properties)
        (let* ((type    (concat "type = \"" (org-entry-get nil "HUGO_TYPE") "\"\n"))
               (date     (concat "date = \""
                                 (format-time-string "%Y-%m-%d"
                                                     (apply 'encode-time
                                                            (org-parse-time-string
                                                             (org-entry-get nil "HUGO_DATE"))) t) "\"\n"))
               (tags     (concat "tags = [ \""
                                 (mapconcat 'identity
                                            (split-string
                                             (org-entry-get nil "HUGO_TAGS")
                                             "\\( *, *\\)" t) "\", \"") "\" ]\n"))
               (fm (concat "+++\n"
                           title
    		       type
                           date
                           tags
                           topics
                           "+++\n\n"))
               (coding-system-for-write buffer-file-coding-system)
               (backend  'md)
               (blog))
          ;; try to load org-mode/contrib/lisp/ox-gfm.el and use it as backend
          (if (require 'ox-gfm nil t)
              (setq backend 'gfm)
            (require 'ox-md))
          (setq blog (org-export-as backend t))
          ;; Normalize save file path
          (unless (string-match "^[/~]" file)
            (setq file (concat hugo-content-dir file))
            (unless (string-match "\\.md$" file)
              (setq file (concat file ".md")))
            ;; save markdown
            (with-temp-buffer
              (insert fm)
              (insert blog)
              (untabify (point-min) (point-max))
              (write-file file)
              (message "Exported to %s" file))))))


<a id="orgc531031"></a>

## ox-hugo setup

    (use-package ox-hugo
      :ensure t
      :defer t
      :custom
      (org-hugo--tag-processing-fn-replace-with-hyphens-maybe t)
      )


<a id="orgdc58a72"></a>

# Scimax customisations

These are settings which custmise scimax specific variables. These are separated out here so that it becomes easier to try out Emacs configurations that are outside scimax.


<a id="org69220a5"></a>

## Scimax Hotspots

    (defun hotspots ()
      "helm interface to my hotspots, which includes my locations,
    org-files and bookmarks"
      (interactive)
      (helm :sources `(((name . "Mail and News")
                        (candidates . (("Agenda All" . (lambda () (org-agenda "" "a")))
                                       ("Agenda Office" . (lambda () (org-agenda "" "o")))
    				   ("Mail" . (lambda ()
                                                   (if (get-buffer "*mu4e-headers*")
                                                       (progn
                                                         (switch-to-buffer "*mu4e-headers*")
                                                         (delete-other-windows))
                                                     (mu4e))))
                                       ("Calendar" .
                                        (lambda ()
                                          (browse-url
                                           "https://www.google.com/calendar/render")))
                                       ("RSS" . elfeed)))
                        (action . (("Open" . (lambda (x) (funcall x))))))
                       ((name . "My Locations")
                        (candidates . (("CV Org" . "~/org_cv/CV_Shreyas_Ragavan.org")
                                       ("scd - scimax dir" . "~/scimax/" )
                                       ("scu - scimax user dir" . "~/scimax/user/")
                                       ( "sco - scimax org conf". "~/scimax/user/sr-config.org")
                                       ("blog" . "~/my_org/blog-book.org")
    				   ("github" . "~/my_gits/")
                                       ("project" . "~/my_projects/")
                                       ("cheatsheet" . "~/my_cheatsheets/")
                                       ("passwords" . "~/my_org/secrets.org.gpg")
                                       ("references" . "~/Dropbox/bibliography/references.bib")
                                       ))
                        (action . (("Open" . (lambda (x) (find-file x))))))

                       ((name . "My org files")
                        (candidates . ,(f-entries "~/my_org"))
                        (action . (("Open" . (lambda (x) (find-file x))))))
                       helm-source-recentf
                       helm-source-bookmarks
                       helm-source-bookmark-set)))


<a id="orgdcef3a7"></a>

## Scimax Elfeed

    (require 'scimax-elfeed)


<a id="org816c62c"></a>

## Scimax Notebook directory

    (setq nb-notebook-directory "~/my_projects/")


<a id="orgef72ee1"></a>

## Scimax notebook

    (global-set-key (kbd "M-s n") 'nb-open)


<a id="org0bb7f26"></a>

## TODO Scimax Python

    (require 'scimax-org-babel-python)
    (require 'ob-ipython)
    (require 'scimax-ob)
    (require 'scimax-org-babel-ipython-upstream)
    (setq ob-ipython-exception-results nil)
    (scimax-ob-ipython-turn-on-eldoc)


<a id="org88f4557"></a>

## TODO Bibliography settings and customisation

This was setup a long time ago to convert past technical repots into org mode, with references made in correct technical style. This project is on hold.

    (require 'doi-utils)
    (require 'org-ref-wos)
    (require 'org-ref-pubmed)
    (require 'org-ref-arxiv)
    (require 'org-ref-bibtex)
    (require 'org-ref-pdf)
    (require 'org-ref-url-utils)
    (require 'org-ref-helm)

    ;; note and bib location

    (setq org-ref-bibliography-notes "~/my_org/references/references.org"
          org-ref-bibliography-notes "~/my_org/references/research_notes.org"
          org-ref-default-bibliography '("~/my_org/references/references.bib")
          org-ref-pdf-directory "~/my_org/references/pdfs/")

    ;; setting up helm-bibtex
    (setq helm-bibtex-bibliography "~/my_org/references/references.bib"
          helm-bibtex-library-path "~/my_org/org/references/pdfs"
          helm-bibtex-notes-path "~/my_org/references/research_notes.org")


<a id="org4b969da"></a>

# Python <code>[0/4]</code>


<a id="org2f76795"></a>

## Using miniconda


<a id="org9ba170b"></a>

## NEXT setup virtual environment approach


<a id="org9870a98"></a>

## NEXT setup conda, especially for auto complete


<a id="orged42586"></a>

## General config

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 14:52] </span></span>
    This is to take care of the annoying indentation message that always pops up.

    (setq python-indent-guess-indent-offset nil)


<a id="org5d72539"></a>

## NEXT Autocomplete for python blocks

    (add-to-list 'company-backends 'company-ob-ipython)
    (company-mode)


<a id="org40db753"></a>

## POSTPONED Emacs-jupyter

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 14:48] </span></span>
    Since I am more familiar with ob-ipython and there are a bunch of interesting features already implemented in it like the automatic setting of a kernel and file names for graphic outputs and so on - I will explore jupyter-emacs at a later date.

    (use-package jupyter
      :ensure t
      :defer t
      :config
      (org-babel-load-languages '(jupyter .t))
      (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                           (:session . "jipython")
                                                           (:kernel . "python3")))
      )


<a id="orgd95469f"></a>

### Test for jupyter-python

    import pandas as pd


<a id="orgf194008"></a>

# TODO Project publishing setup <code>[0/2]</code>

This is under construction and was initially started with the idea of having custom publishing settings for different projects. I was initially looking at this for publishing my hugo blog. However, the need has been negated with the excellent ox-hugo package.


<a id="orgc661e56"></a>

## TODO Exporting org projects

    (
     setq org-publish-project-alist
     '(
       ("org-repo"
        :base-directory "./"
        :base-extension "org"
        :publishing-directory "/Users/shreyas/my_projects/dotemacs"
        :EXPORT_FILE_NAME "README.org"
        :recursive f
        :publishing-function org-html-publish-to-html
        ;; :html-head "<link rel="stylesheet" href="http://dakrone.github.io/org2.css" type="text/css" />"
        )

       ("md"
        :base-directory "./"
        :base-extension "org"
        :publishing-directory "./export/"
        :recursive t
        :publishing-function org-md-export-to-markdown
        )

       ("Documentation - html + md"
        :components ("html-static" "md" )
        )))


<a id="org9b3bbaf"></a>

## TODO Function for exporting dotemacs config <code>[1/3]</code>

-   [ ] Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-14 Thu 14:05] </span></span>
    Save the filename as variables.
-   [X] Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-14 Thu 13:30] </span></span>
    Add a condition to check if the directory exists.
-   [ ] Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-10 Sun 07:16] </span></span>
    Add a line to revert target export files if they are open. Prefer exporting the org file rather than copying.

This is the beginning of a function to perform 3 exports:

1.  Export to my hugo website as a part of my documentation (ox-hugo)
2.  Copy the org file to my github repo.
3.  Tangle the copied org file to the above github repository to have the script ready.

Maintaining the documentation on my website does not make it easy to others to view the changes in the configuration and fork or download the same as an org file or emacs-lisp script. Therefore the config that I publish should be maintained in it's own repository.

As of now, I'm calling this function from my Emacs config file, and need to improve the above workflow.

    (defun sr/dotemacs-export()
      (interactive)
      "If directories exist - exporting Org config to Hugo blog, and to Github repository org file and lisp"

      (if (file-directory-p "~/my_projects/dotemacs")
          (progn
            (copy-file "~/scimax/user/sr-config.org" "~/my_projects/dotemacs/README.org" "OK-IF-ALREADY-EXISTS")
            (copy-file "~/scimax/user/sr-config.el" "~/my_projects/dotemacs/config.el" "OK-IF-ALREADY-EXISTS")
            ;; (org-babel-tangle-file  "~/my_projects/dotemacs/README.org" "~/my_projects/dotemacs/config.el")
            )
        )
      (if (file-directory-p "~/my_gits/hugo-sr")
          (progn
            (org-hugo-export-to-md)
            )
        )
      )


<a id="org49ef83b"></a>

# TODO mu4e

-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-12 Tue 14:53] </span></span>
    The use-package documentation specifies a method to do this via use-package itself, without enclosing the whole snippet within a if clause.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 20:43] </span></span>
    The mu4e config has to be broken down and the send email with htmlize has to be evaluated.
-   Note taken on <span class="timestamp-wrapper"><span class="timestamp">[2019-02-07 Thu 09:04] </span></span>
    As of now, I do not acess my email on different computers via Emacs. The end goal is to setup a mail server via VPS and store my email online, which can then be searched via Emacs and mu4e from any location.

    (if (system-type-is-darwin)
        (progn
          (use-package mu4e
            :ensure nil
            :config
            (require 'mu4e)
            (require 'mu4e-contrib)
            (require 'org-mu4e)

            (setq
             mue4e-headers-skip-duplicates  t
             mu4e-view-show-images t
             mu4e-view-show-addresses 't
             mu4e-compose-format-flowed nil
             mu4e-update-interval 200
             message-ignored-cited-headers 'nil
             mu4e-date-format "%y/%m/%d"
             mu4e-headers-date-format "%Y/%m/%d"
             mu4e-change-filenames-when-moving t
             mu4e-attachments-dir "~/Downloads/Mail-Attachments/"
             mu4e-maildir (expand-file-name "~/my_mail/fmail")
             )

            ;; mu4e email refiling loations
            (setq
             mu4e-refile-folder "/Archive"
             mu4e-trash-folder  "/Trash"
             mu4e-sent-folder   "/Sent"
             mu4e-drafts-folder "/Drafts"
             )

            ;; setup some handy shortcuts
            (setq mu4e-maildir-shortcuts
                  '(("/INBOX"   . ?i)
    	        ("/Sent"    . ?s)
    	        ("/Archive" . ?a)
    	        ("/Trash"   . ?t)))

            ;;store link to message if in header view, not to header query
            (setq org-mu4e-link-query-in-headers-mode nil
                  org-mu4e-convert-to-html t) ;; org -> html

            ;; Enabling view in browser for HTML heavy emails that don't render well
            (add-to-list 'mu4e-view-actions
    	             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

            (autoload 'mu4e "mu4e" "mu for Emacs." t)

            ;; Config for sending email
            (setq
             message-send-mail-function 'message-send-mail-with-sendmail
             send-mail-function 'sendmail-send-it
             message-kill-buffer-on-exit t
             )

            ;; allow for updating mail using 'U' in the main view:
            (setq mu4e-get-mail-command  "mbsync -a -q")

            ;; Don't keep asking for confirmation for every action
            (defun my-mu4e-mark-execute-all-no-confirm ()
              "Execute all marks without confirmation."
              (interactive)
              (mu4e-mark-execute-all 'no-confirm))
            ;; mapping x to above function
            (define-key mu4e-headers-mode-map "x" #'my-mu4e-mark-execute-all-no-confirm)
            )
          ;; source: http://matt.hackinghistory.ca/2016/11/18/sending-html-mail-with-mu4e/

          ;; this is stolen from John but it didn't work for me until I
          ;; made those changes to mu4e-compose.el
          (defun htmlize-and-send ()
            "When in an org-mu4e-compose-org-mode message, htmlize and send it."
            (interactive)
            (when
                (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
              (org-mime-htmlize)
              (org-mu4e-compose-org-mode)
              (mu4e-compose-mode)
              (message-send-and-exit)))

          ;; This overloads the amazing C-c C-c commands in org-mode with one more function
          ;; namely the htmlize-and-send, above.
          (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)
          )
      )
