(let ((default-directory  "~/scimax/user/external_packages/"))
  (normal-top-level-add-subdirs-to-load-path))

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

(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead in the mac
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install)
)

(use-package better-defaults
  :ensure t
)

(message "Loaded better-defaults package")

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ivy-initial-inputs-alist nil)

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

;;fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

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

(global-set-key (kbd "M-s g") 'google-this-mode-submap)

(global-set-key (kbd "M-s i") 'ivy-yasnippet)

(global-set-key (kbd "M-s u") 'mu4e-update-mail-and-index)
(global-set-key (kbd "M-s m") 'mu4e~headers-jump-to-maildir)
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

(global-set-key (kbd "C-x t") 'org-insert-todo-heading)
(global-set-key (kbd "C-c d") 'org-time-stamp)
(global-set-key (kbd "M-s s") 'org-save-all-org-buffers)
(global-set-key (kbd "M-s j") 'org-journal-new-entry)

(global-set-key (kbd "C-<f9>") 'sr/punch-in)
(global-set-key (kbd "M-<f9>") 'sr/punch-out)

(global-set-key (kbd "M-s t") 'treemacs-switch-workspace)

(global-set-key (kbd "M-s f") 'frog-jump-buffer)

(defun my/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][link]]"))
(global-set-key (kbd "<f6>") 'my/yank-more)

(require 'ox-org)
(require 'ox-word)
(require 'ox-md)
(load "~/scimax/ox-ipynb/ox-ipynb.el")

(setq markdown-command "pandoc")

(use-package org-bookmark-heading
  :ensure t
  :defer t
  :config
  (require 'org-bookmark-heading)
)

(use-package ob-async
  :ensure t
  )

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(semantic-mode 1)

(message "Loaded Emacs general config")

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

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

(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)

;; advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter)
  )
(advice-add 'swiper :after #'bjm-swiper-recenter)

(message "Loaded Swiper customisation")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(message "Loaded easier selection")

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(setq magit-revert-buffers 'silent)

(use-package git-timemachine
  :ensure t)

(message "Loaded git related config")

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

(use-package ess
  :ensure t
  :defer t
  :config
  (require 'ess)
  (setq ess-describe-at-point-method nil)
  (setq ess-switch-to-end-of-proc-buffer t)
  (setq ess-rutils-keys +1)
  (setq ess-eval-visibly 'nil)
  (setq ess-use-flymake +1)
  (setq ess-use-company t)
  (setq ess-history-file "~/.Rhistory")
  (setq ess-use-ido t)
  (setq ess-roxy-hide-show-p t)
  ;;(speedbar-add-supported-extension ".R")
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  )

;;(require 'ess-R-data-view)
;;(require 'ess-rutils)

(use-package ess-view
  :ensure t
  :defer t
  :config
  (if (system-type-is-darwin)
      (setq ess-view--spreadsheet-program
            "/Applications/Tad.app/Contents/MacOS/Tad"
            )
    )
  (if (system-type-is-gnu)
      (setq ess-view--spreadsheet-program
            "tabview"
            )
    )
  )


(message "Loaded ESS configuration")

(setq display-buffer-alist
      `(("*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.33)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-width . 0.35)
         (reusable-frames . nil))
        ("*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.33)
         (reusable-frames . nil))))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(message "Loaded MC")

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

(setq
 org-directory "~/my_org/"
 org-agenda-files '("~/my_org/")
 )

(setq org-log-state-notes-insert-after-drawers t)
(setq org-log-redeadline 'time)

(require 'org-capture)
(require 'org-protocol)

(add-hook 'find-file-hooks
          (lambda ()
            (let ((file (buffer-file-name)))
              (when (and file (equal (file-name-directory file) "~/my_org/archive/"))
                (org-mode)))))

(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))

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

(setq org-id-method (quote uuidgen))
(add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "CANCEL(c)" "POSTPONED(p)" "|" "DONE(d)" "STABLE(s)")
        (sequence "TEST(T)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" )))

(setq org-refile-targets
      '((nil :maxlevel . 4)
        (org-agenda-files :maxlevel . 4)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-reverse-note-order t)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-agenda-start-on-weekday 1)

(setq org-agenda-tags-column -150)

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

;; (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
;;   (setq org-agenda-file-regexp
;;         (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
;;                                   org-agenda-file-regexp)))

(setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")

(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq org-agenda-text-search-extra-files (apply 'append
						(mapcar
						 (lambda (directory)
						   (directory-files-recursively
						    directory org-agenda-file-regexp))
						 '("~/my_projects/" "~/my_brain/"))))

(setq org-agenda-search-view-always-boolean t)

(setq org-agenda-sticky t)

(require 'org-habit)
(setq org-habit-graph-column 90)

(setq org-capture-templates
      '(("t" "Task entry")
        ("tt" "Todo - Fast" entry (file+headline "~/my_org/todo-global.org" "@Inbox")
	 "** TODO %?")
        ("tb" "Todo -BGR" entry (file+headline "~/my_org/bgr.org" "#BGR #Inbox")
	 "** TODO %?")
        ("te" "Todo - Emacs" entry (file+headline "~/my_org/todo-global.org" "@Emacs notes and tasks")
         "** TODO %?")
        ("td" "Datascience inbox" entry (file+headline "~/my_org/datascience.org" "@Datascience @Inbox")
         "** TODO %?")
	("tm" "Mail Link Todo" entry (file+headline "~/my_org/todo-global.org" "@Inbox")
	 "** TODO Mail: %a ")
        ("l" "Link/Snippet" entry (file+headline "~/my_org/link_database.org" ".UL Unfiled Links")
         "** %? %a ")
        ("e" "Protocol info" entry ;; 'w' for 'org-protocol'
         (file+headline "~/my_org/link_database.org" ".UL Unfiled Links")
         "*** %a, \n %:initial")
        ("n" "Notes")
        ("ne" "Emacs note" entry (file+headline "~/my_org/todo-global.org" "@Emacs notes and tasks")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("nn" "General note" entry (file+headline "~/my_org/notes.org" "@NOTES")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("nd" "Datascience note" entry (file+headline "~/my_org/datascience.org" "@Datascience @Notes")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("g" "BGR stuff")
        ("gi" "Inventory project")
        ("gil" "Daily log" entry (file+olp+datetree "~/my_org/bgr.org" "Inventory management Project") "** %? %i")
        ("C" "Commandment" entry (file+datetree "~/my_org/lifebook.org" "")
         "** %? %i :commandment:")
        ("J" "Job search" entry (file+headline "~/my_org/mrps_canjs.org" "MRPS #CANJS")
         "** TODO %? %i ")
        ("w" "Website" plain
         (function org-website-clipper)
         "* %a %T\n" :immediate-finish t)
        ("j" "Journal entry" entry (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format) %?")
        ("i" "Whole article capture" entry
         (file+headline "~/my_org/full_article_archive.org" "" :empty-lines 1)
         "** %a, %T\n %:initial" :empty-lines 1)
        ("c" "Clocking capture")
        ("ct" "Clock TODO" entry (clock) "** TODO %?")
        ("cn" "Clock Note" entry (clock) "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
        ("r" "Review note" entry (file+weektree "~/my_org/lifebook.org" "#Personal #Reviews")
         "** %?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:")
         ))

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))))

(setq delete-old-versions -1)
(setq version-control t)

(use-package org-noter
  :ensure t
  :defer t
  :config
  (setq org-noter-set-auto-save-last-location t)
  )

(use-package org-projectile
  :ensure t
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (setq org-projectile-projects-file
        "~/my_org/project-tasks.org")
  ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))) ;; Not necessary as my task projects are a part of the main org folder
  (push (org-projectile-project-todo-entry) org-capture-templates)
  )

(defun sr/log-todo-creation-date (&rest ignore)
  "Log TODO creation time in the property drawer under the key 'CREATED'."
  (when (and (org-get-todo-state)
             (not (org-entry-get nil "CREATED")))
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a]"))
    (org-entry-put nil "PLANNED" (format-time-string (cdr org-time-stamp-formats)))
    ))

(advice-add 'org-insert-todo-heading :after #'sr/log-todo-creation-date)
(advice-add 'org-insert-todo-heading-respect-content :after #'sr/log-todo-creation-date)
(advice-add 'org-insert-todo-subheading :after #'sr/log-todo-creation-date)
(advice-add 'org-capture :after #'sr/log-todo-creation-date)
(advice-add 'org-projectile-project-todo-completing-read :after #'sr/log-todo-creation-date)

;; (require 'org-expiry)
;; ;; Configure it a bit to my liking
;; (setq
;;  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
;;  org-expiry-inactive-timestamps   nil         ; Don't have everything in the agenda view
;;  )

;; (defun mrb/insert-created-timestamp()
;;   "Insert a CREATED property using org-expiry.el for TODO entries"
;;   (org-expiry-insert-created)
;;   (org-back-to-heading)
;;   (org-end-of-line)
;;   (insert " ")
;;   )

;; ;; Whenever a TODO entry is created, I want a timestamp
;; ;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
;; (defadvice org-insert-todo-heading (after mrb/created-timestamp-advice activate)
;;   "Insert a CREATED property using org-expiry.el for TODO entries"
;;   (mrb/insert-created-timestamp)
;;   )
;; ;; Make it active
;; (ad-activate 'org-insert-todo-heading)

;; (require 'org-capture)

;; (defadvice org-capture (after mrb/created-timestamp-advice activate)
;;   "Insert a CREATED property using org-expiry.el for TODO entries"
;;    					; Test if the captured entry is a TODO, if so insert the created
;;    					; timestamp property, otherwise ignore
;;   (mrb/insert-created-timestamp))
;; ;;  (when (member (org-get-todo-state) org-todo-keywords-1)
;; ;;    (mrb/insert-created-timestamp)))
;;   (ad-activate 'org-capture)

;; Add feature to allow easy adding of tags in a capture window
(defun mrb/add-tags-in-capture()
  (interactive)
  "Insert tags in a capture window without losing the point"
  (save-excursion
    (org-back-to-heading)
    (org-set-tags)))
;; Bind this to a reasonable key
(define-key org-capture-mode-map "\C-c\C-t" 'mrb/add-tags-in-capture)

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (scheme . t)
   (sqlite . t)
   (R . t)
   ;(jupyter . t)
   )
 )

(require 'cider)
(setq org-babel-clojure-backend 'cider)

(defvar sr/organization-task-id "a8712a47-a648-477f-bdbf-d6004a0cc70b")

(defun sr/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find sr/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun sr/punch-in (arg)
    (interactive "p")
  (setq sr/keep-clock-running t)
  (sr/clock-in-organization-task-as-default))

(defun sr/punch-out ()
  (interactive)
  (setq sr/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  )

(defun sr/clock-out-maybe ()
  (when (and sr/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (sr/clock-in-organization-task-as-default)))

(add-hook 'org-clock-out-hook 'sr/clock-out-maybe 'append)

(use-package org-mru-clock
  :ensure t
  :bind (("M-s 1" . org-mru-clock-in)
          ("C-c C-x C-j" . org-mru-clock-select-recent-task))
  :init
  (setq org-mru-clock-how-many 100
        org-mru-clock-completing-read #'ivy-completing-read))

(setq org-clock-out-remove-zero-time-clocks t)

;; setting idle timer to 15 minutes
(setq org-clock-idle-time 15)

;; Show lot of clocking history so it's easy to pick items off the `C-c I` list
(setq org-clock-history-length 23)

(defun eos/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

(global-set-key (kbd "C-c I") #'eos/org-clock-in)
(global-set-key (kbd "C-c O") #'org-clock-out)

(use-package org-brain
  :ensure t
  :init
  (setq org-brain-path "~/my_org/brain/")
  ;; ;; For Evil users
  ;; (with-eval-after-load 'evil
  ;;   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/scimax/user/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (add-hook 'org-brain-refile 'org-id-get-create)
  )

(defun org-brain-deft ()
  "Use `deft' for files in `org-brain-path'."
  (interactive)
  (let ((deft-directory org-brain-path)
        (deft-recursive t)
        (deft-extensions '("org")))
    (deft)))

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/my_org/journal/")
  (org-journal-file-format "%Y%m%d")
  (org-journal-enable-agenda-integration t)
  )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(use-package org-sticky-header
  :ensure t
  :config
  (org-sticky-header-mode)
  )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs
          (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package sauron
  :ensure t
  :config
  (require 'sauron)
  (setq sauron-modules '(sauron-org sauron-notifications))
  )

(use-package deft
  :bind ("<f8> d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/my_brain/"
                deft-extensions '("md" "org" "txt")
                deft-recursive t
                ))

(use-package helm-ext
  :ensure t
  :config
  (helm-ext-ff-enable-skipping-dots t)
  ;; Testing the auto path expansion
  ;;(helm-ff-ext-enable-auto-path-expansion t)
  )

(global-set-key (kbd "M-x") 'helm-M-x)
;; Enable fuzzy match for helm-M-x
(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-mini)

(require 'helm-config)
(helm-mode 1)

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

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)

(custom-set-variables
 '(helm-follow-mode-persistent t))

(require 'helm-ag)
(require 'helm-org-rifle)
(global-set-key (kbd "C-c C-w") #'helm-org-rifle--refile)

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

(message "Loaded Helm customisations")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(setq scheme-program-name "/Applications/MIT-GNU-Scheme.app/Contents/Resources/mit-scheme")
(require 'xscheme)

(message "Loaded scheme setup")

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

(defun helm-do-ag-projects ()
  "Grep string in Project directory" (interactive)
  (let ((rootdir (concat "~/my_projects/")))
    (let ((helm-ag-command-option (concat helm-ag-command-option "")))
      (helm-do-ag rootdir))))

(defun helm-do-ag-emacs-config ()
  "Grep string in Emacs custom code"
  (interactive)
  (let ((rootdir (concat "~/scimax/user/sr-cust/")))
    (let ((helm-ag-command-option (concat helm-ag-command-option "")))
      (helm-do-ag rootdir))))

(defun helm-do-ag-journal ()
  "Grep string in journal"
  (interactive)
  (let ((specfile (concat "~/my_org/journal/")))
    (let ((helm-ag-command-option (concat helm-ag-command-option "")))
      (helm-ag-this-file rootdir))))

(defun helm-do-ag-bgr ()
  "Grep string in BGR file"
  (interactive)
  (let ((specfile (concat "~/my_org/bgr.org")))
    (let ((helm-ag-command-option (concat helm-ag-command-option "")))
      (helm-do-ag-this-file specfile))))

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

;; my_org magit status
(defun sr/windows-magit-org ()
  (interactive)
  (ace-delete-other-windows)
  (magit-status "~/my_org/")
  )

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

(message "Loaded Hydras")

;; Elfeed configuration source :
(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("A" . bjm/elfeed-show-all)
              ("E" . bjm/elfeed-show-emacs)
              ("D" . bjm/elfeed-show-daily)
              ("q" . bjm/elfeed-save-db-and-bury))
  :init
  (setq my/default-elfeed-search-filter "@1-month-ago +unread !sport ")
  (setq-default elfeed-search-filter my/default-elfeed-search-filter)
  (setq elfeed-db-direcory "~/scimax/user/elfeeddb")
  :config
  (elfeed-org)
  (elfeed-goodies/setup)

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
             ("v" . elfeed-search-quick-url-note))

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
  )

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

(message "Loaded Elfeed customisations")

;;when I want to enter the web address all by hand
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address(default: w3m-home):" nil nil w3m-home-page nil )))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(eval-after-load 'w3m
  '(progn
     (define-key w3m-mode-map "q" 'w3m-previous-buffer)
     (define-key w3m-mode-map "w" 'w3m-next-buffer)
     (define-key w3m-mode-map "x" 'w3m-close-window)))

(defun wicked/w3m-open-current-page-in-default-browser ()
  "Open the current URL in Mozilla Firefox."
  (interactive)
  (browse-url-default-browser w3m-current-url)) ;; (1)

(defun wicked/w3m-open-link-or-image-in-default-browser ()
  "Open the current link or image in Firefox."
  (interactive)
  (browse-url-default-browser (or (w3m-anchor) ;; (2)
                                         (w3m-image)))) ;; (3)

(eval-after-load 'w3m
  '(progn
     (define-key w3m-mode-map "o" 'wicked/w3m-open-current-page-in-default-browser)
     (define-key w3m-mode-map "O" 'wicked/w3m-open-link-or-image-in-default-browser)))

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

(defun hn ()
  (interactive)
  (browse-url "http://news.ycombinator.com"))

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

(defvar hugo-content-dir "~/my_gits/hugo-sr/content/post/"
  "Path to Hugo's content directory")

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

(use-package ox-hugo
  :ensure t
  :defer t
  :after ox
  :custom
  (org-hugo--tag-processing-fn-replace-with-hyphens-maybe t)
  )

(org-babel-lob-ingest "~/my_projects/sr-snip-lob/README.org")

(add-hook 'org-mode-hook 'scimax-autoformat-mode)
(scimax-toggle-abbrevs 'scimax-month-abbreviations +1)
(scimax-toggle-abbrevs 'scimax-transposition-abbreviations +1)
(scimax-toggle-abbrevs 'scimax-misc-abbreviations nil)
(scimax-toggle-abbrevs 'scimax-weekday-abbreviations +1)
(global-set-key (kbd "s-q") 'org-latex-math-region-or-point)

(setq scimax-user-hotspot-commands
      '(("Agenda All" . (lambda () (org-agenda "" "a")))
        ("Agenda Office" . (lambda () (org-agenda "" "o")))
	("Mail" . (lambda ()
                    (if (get-buffer "*mu4e-headers*")
                        (progn
                          (switch-to-buffer "*mu4e-headers*")
                          (delete-other-windows))
                      (mu4e))))
        ("Bookmarks" . (lambda () (helm-source-bookmarks)))
        ("Reload Scimax babel" . (lambda () (org-babel-load-file (expand-file-name "sr-config.org" user-emacs-directory))))
        )
      )

(setq scimax-user-hotspot-locations
      '(
        ("CV Org" . "~/org_cv/CV_Shreyas_Ragavan.org")
        ("scd - scimax dir" . "~/scimax/" )
        ("scu - scimax user dir" . "~/scimax/user/")
        ( "sco - scimax org conf" . "~/scimax/user/sr-config.org")
        ("blog" . "~/my_org/blog-book.org")
	("github" . "~/my_gits/")
        ("project" . "~/my_projects/")
        ("cheatsheet" . "~/my_cheatsheets/")
        ("passwords" . "~/my_org/secrets.org.gpg")
        ("references" . "~/Dropbox/bibliography/references.bib")
        )
      )

(require 'scimax-elfeed)

(setq nb-notebook-directory "~/my_projects/")

(global-set-key (kbd "M-s n") 'nb-open)

(require 'scimax-statistics)

(require 'scimax-org-babel-python)
(require 'ob-ipython)
(require 'scimax-ob)
(require 'scimax-org-babel-ipython-upstream)
(setq ob-ipython-exception-results nil)
(scimax-ob-ipython-turn-on-eldoc)

(setq python-indent-guess-indent-offset nil)

(if (system-type-is-darwin)
    (progn
      ;;; Code:
      (defun make-orgcapture-frame ()
        "Create a new frame and run org-capture."
        (interactive)
        (make-frame '((name . "remember") (width . 80) (height . 16)
                      (top . 400) (left . 300)
                      (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                      ))
        (select-frame-by-name "remember")
        (org-capture))
      )
  )

(use-package ox-tufte
  :defer t
  :config
  (require 'ox-tufte)
  )

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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
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

(use-package pocket-reader
  :ensure t
  :config
  (require 'pocket-reader)
)

(use-package frog-menu
  :ensure t
  :config
  :defer t
)
(load "frog-jump-buffer")

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  )

(setq default-frame-alist
      '(;; (background-color . "whitesmoke")
        ;; (foreground-color . "black")
        (fullscreen . maximized)
        ))

(setq custom-safe-themes t)
;; (set-background-color "whitesmoke")

(disable-theme 'leuven)
;;(load-theme 'spacemacs-dark t)
(load-theme 'zenburn t)

;; use variable-pitch fonts for some headings and titles
(setq zenburn-use-variable-pitch t)

;; scale headings in org-mode
(setq zenburn-scale-org-headlines t)

;; scale headings in outline-mode
(setq zenburn-scale-outline-headlines t)

;; For Linux
(if (system-type-is-gnu)
    (set-face-attribute 'default nil :family "ttf-iosevka" :height 140))

;; For Mac OS
(if (system-type-is-darwin)
    (set-face-attribute 'default nil :family "Iosevka Type" :height 160 ))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.00))))
 '(org-level-5 ((t (:inherit outline-5 :height .95))))
 )

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (disable-theme 'smart-mode-line-light)
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-toggle-buffer-position-off)
  )

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

;; ;; use org bullets from emacsist
;; (use-package org-bullets
;;   :ensure t
;;   :init
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
			     :weight normal
			     :strike-through t))))
 '(org-headline-done
   ((((class color) (min-colors 16) (background dark))
     (:foreground "LightSalmon" :strike-through t)))))

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

;;(set-face-background 'org-block-emacs-lisp "black")
(set-face-background 'org-block "black")

(use-package visual-fill-column
  :ensure t
  :config
  (global-visual-fill-column-mode)
  (setq-default fill-column 80)
  (setq-default visual-fill-column-center-text t)
  (setq split-window-preferred-function
        'visual-fill-column-split-window-sensibly)
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook 'turn-on-visual-fill-column-mode)
  )
