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

(use-package browse-kill-ring
  :ensure t
  :defer nil
)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ivy-initial-inputs-alist nil)

;; sentences end with single space
(setq sentence-end-double-space nil)

(setq package-list '(diminish
                     ;; ztree
                     ;; org-gcal
                     w3m
                     ;; org-trello
                     org-web-tools
                     auto-indent-mode
                     ob-sql-mode
                     dash
                     org-super-agenda
		     ;; workgroups2
                     switch-window
                     ess
                     ess-R-data-view
                     ;; interleave
                     deft
                     org-bookmark-heading
                     writeroom-mode
                     ;; evil
                     ;; evil-leader
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
                     ;; ein-mumamo
                     ido-vertical-mode
                     company-jedi
                     conda
                     ;; spacemacs-theme
                     ;; elfeed-goodies
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
(set-register ?b (cons 'file "~/my_org/blog-book.org"))
(set-register ?g (cons 'file "~/my_gits/"))

(global-set-key (kbd "M-s g") 'google-this-mode-submap)

(global-set-key (kbd "M-s i") 'ivy-yasnippet)

(global-set-key (kbd "M-s u") 'mu4e-update-mail-and-index)
(global-set-key (kbd "M-s m") 'mu4e~headers-jump-to-maildir)
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

(global-set-key (kbd "C-x t") 'org-insert-todo-heading)
(global-set-key (kbd "C-c d") 'org-time-stamp)
(global-set-key (kbd "M-s s") 'org-save-all-org-buffers)
;;(global-set-key (kbd "M-s j") 'org-journal-new-entry)

(global-set-key (kbd "C-<f9>") 'sr/punch-in)
(global-set-key (kbd "M-<f9>") 'sr/punch-out)

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

(use-package slime
  :ensure t
  )

(if (system-type-is-darwin)
    (setq inferior-lisp-program "/usr/local/bin/clisp")
  )

(if (system-type-is-gnu)
    (setq inferior-lisp-program "/usr/bin/clisp")
    )

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(message "Loaded easier selection")

(global-set-key (kbd "M-/") (make-hippie-expand-function
			     '(try-expand-dabbrev-visible
			       try-expand-dabbrev
			       try-expand-dabbrev-all-buffers) t))

(setq default-frame-alist
      '(;; (background-color . "whitesmoke")
        ;; (foreground-color . "black")
        (fullscreen . maximized)
        ))

(setq custom-safe-themes t)
(set-background-color "whitesmoke")

;; For Linux
(if (system-type-is-gnu)
    (set-face-attribute 'default nil :family "ttf-iosevka" :height 130 ))

;; For Mac OS
(if (system-type-is-darwin)
    (set-face-attribute 'default nil :family "Iosevka Type" :height 160 ))

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
 '(org-done ((t (:foreground "DarkGreen"
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

(if (system-type-is-darwin)
    (progn
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
       message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote..."
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
      ))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(message "Loaded MC")

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(setq magit-revert-buffers 'silent)

(message "Loaded git related config")

(setq projectile-sort-order 'recently-active)

;; Change cache file location
(setq projectile-cache-file "~/my_org/emacs_meta/.projectile-cache")

(global-set-key (kbd "M-x") 'helm-M-x)
;; Enable fuzzy match for helm-M-x
(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") #'helm-mini)

(require 'helm-config)
(require 'helm-for-files)
(helm-mode 1)

(setq bookmark-default-file "~/my_org/emacs_meta/bookmarks")

(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x r l") #'helm-bookmarks)
(setq helm-bookmark-show-location t)

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

(use-package helm-ag
  :ensure t
  :defer nil
  :config
  (require 'helm-ag)
)

(use-package helm-org-rifle
  :ensure t
  :defer nil
  :config
  (require 'helm-org-rifle)
  (global-set-key (kbd "C-c C-w") #'helm-org-rifle--refile)
)

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

(setq org-complete-tags-always-offer-all-agenda-tags t)

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

(require 'org-id)
(setq org-id-link-to-org-use-id t)
(org-link-set-parameters "id" :store #'org-id-store-link)
(org-link-set-parameters "nb" :store nil)
;; Update ID file .org-id-locations on startup
;; This adds too much time to startup
;; (org-id-update-id-locations)

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
						 '("~/my_projects/" "~/my_org/brain/"))))

(setq org-agenda-text-search-extra-files '(agenda-archives))

(setq org-agenda-search-view-always-boolean t)

(setq org-agenda-sticky t)

(require 'org-habit)
(setq org-habit-graph-column 90)

(setq org-datetree-add-timestamp nil)

(setq org-capture-templates
      '(("t" "Task entry")
        ("tt" "Todo - Fast" entry (file+headline "~/my_org/todo-global.org" "@Inbox")
	 "** TODO %?")
        ("tj" "Todo -Job journal" entry (file+olp+datetree "~/my_org/ds-jobs.org" "Job Search Journal")
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
   (lisp . t)
   (sql .  t)
   ;(jupyter . t)
   )
 )

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
  (setq org-id-locations-file "~/my_org/emacs_meta/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?\n:PROPERTIES:\n:CREATED: [%<%Y-%m-%d %a %H:%M>]\n:END:" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (add-hook 'org-brain-refile 'org-id-get-create)
  (global-set-key (kbd "M-s v") #'org-brain-visualize)
  )

(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/my_org/journal/")
  (org-journal-file-format "%Y%m%d")
  (org-journal-enable-agenda-integration t)
  )

(use-package org-sticky-header
  :ensure t
  :config
  (org-sticky-header-mode)
  )

(use-package ox-rst
  :ensure t
  :defer t
  :config
  (require 'ox-rst)
  )

(use-package ox-slack
  :ensure t
  :defer
  :config
  (require 'ox-slack)
  )

(use-package ox-pandoc
  :ensure t
  :defer
  :config
  (require 'ox-pandoc)
  )

(setq org-columns-default-format "%50ITEM %TODO %3PRIORITY %10TAGS %17Effort(Estimated Effort){:} %12CLOCKSUM")

(use-package org-sidebar
  :ensure t
  :defer nil
  )

(use-package git-timemachine
  :ensure t)

(let ((default-directory  "~/scimax/user/external_packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(use-package memento-mori
  :ensure t
  :defer nil
  :config
  (setq memento-mori-birth-date "2018-12-31")
  (memento-mori-mode 1)
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
          ttreemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  "~/my_org/emacs_meta/.treemacs-persist"
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

    ;;(treemacs-follow-mode t)
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
        ("M-s t t" . treemacs)
        ("M-s t w" . treemacs-switch-workspace)
        ;; ("C-x t 1"   . treemacs-delete-other-windows)
        ;; ("C-x t t"   . treemacs)
        ;; ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ;; ("C-x t M-t" . treemacs-find-tag)
        )
  )

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

(defun explorer (&optional path)
  "Open Finder or Windows Explorer in the current directory."
  (interactive (list (if (buffer-file-name)
			 (file-name-directory (buffer-file-name))
		       (expand-file-name default-directory))))
  (cond
   ((string= system-type "gnu/linux")
    (shell-command "nautilus"))
   ((string= system-type "darwin")
    (shell-command (format "open -b com.apple.finder%s"
			   (if path (format " \"%s\""
					    (file-name-directory
					     (expand-file-name path))) ""))))
   ((string= system-type "windows-nt")
    (shell-command (format "explorer %s"
			   (replace-regexp-in-string
			    "/" "\\\\"
			    path))))))

;; (defun sr/bash (args) ...)

(defun sr/bash (&optional path)
  "Open a bash window.
PATH is optional, and defaults to the current directory.
commands (`scimax-user-bash-app')
"
  (interactive (list (if (buffer-file-name)
			 (file-name-directory (buffer-file-name))
		       (expand-file-name default-directory))))
  (cond
   ((string= system-type "gnu/linux")
    (shell-command "xfce4-terminal"))
   ((string= system-type "darwin")
    (shell-command
     (format "open -b com.apple.iterm2"
	     (if path (format " \"%s\"" (expand-file-name path)) ""))))
   ((string= system-type "windows-nt")
    (shell-command "start \"\" \"%SYSTEMDRIVE%\\Program Files\\Git\\bin\\bash.exe\" --login &"))))

(advice-add 'bash :override #'sr/bash)

(advice-remove 'bash #'scimax-user-bash-app)

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
        ("Reload custom config - org babel" . (lambda () (org-babel-load-file (expand-file-name "sr-config.org" user-emacs-directory))))
        )
      )

(setq scimax-user-hotspot-locations
      '(
        ("CV Org" . "~/org_cv/CV_Shreyas_Ragavan.org")
        ("tmrs"  .  "~/my_org/tmsr.org")
        ("scd - scimax dir" . "~/scimax/" )
        ("scu - scimax user dir" . "~/scimax/user/")
        ( "sco - scimax org conf" . "~/scimax/user/sr-config.org")
        ("blog" . "~/my_org/blog-book.org")
	("github" . "~/my_gits/")
        ("project" . "~/my_projects/")
        ("cheatsheet" . "~/my_projects/ds_cheatsheets/")
        ("passwords" . "~/my_org/secrets.org.gpg")
        ("references" . "~/Dropbox/bibliography/references.bib")
        )
      )

(require 'scimax-elfeed)

(setq nb-notebook-directory "~/my_projects/")

(global-set-key (kbd "M-s n") 'nb-open)

(global-set-key (kbd "C-\\") 'scimax/body)

(require 'scimax-org-babel-python)
(require 'ob-ipython)
(require 'scimax-ob)
(require 'scimax-org-babel-ipython-upstream)
(setq ob-ipython-exception-results nil)
(scimax-ob-ipython-turn-on-eldoc)

(message "Loaded scimax customisations")

(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)

;; advise swiper to recenter on exit
(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter)
  )
(advice-add 'swiper :after #'bjm-swiper-recenter)

(message "Loaded Swiper customisation")

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
  :config
  (require 'ess)
  (use-package ess-R-data-view)
  (use-package polymode)
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

;; The following chunk is taken from: https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/ess/packages.el
;;; Follow Hadley Wickham's R style guide
(setq ess-first-continued-statement-offset 2
      ess-continued-statement-offset 0
      ess-expression-offset 2
      ess-nuke-trailing-whitespace-p t
      ess-default-style 'DEFAULT)


;; Adding Poly-R package

(use-package poly-R
  :ensure t
  )
;; The following chunk is taken from antonio's answer from https://stackoverflow.com/questions/16172345/how-can-i-use-emacs-ess-mode-with-r-markdown
(defun rmd-mode ()
  "ESS Markdown mode for rmd files."
  (interactive)
  (require 'poly-R)
  (require 'poly-markdown)
  (poly-markdown+r-mode))

(use-package ess-view
  :ensure t
  :config
  (require 'ess-view)
  (if (system-type-is-darwin)
      (setq ess-view--spreadsheet-program
            "/Applications/Tad.app/Contents/MacOS/Tad"
            )
    )
  (if (system-type-is-gnu)
      (setq ess-view--spreadsheet-program
            "tad"
            )
    )
  )

;; This is taken and slightly modified from the ESS manual
;; The display config is similar to that of Rstudio

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

(message "Loaded ESS configuration")

(use-package ox-reveal
  :ensure ox-reveal
  :defer nil
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)
  )

(use-package htmlize
  :ensure t)

(message "Loaded ox-reveal cust")

(use-package deft
  :bind ("<f8> d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/my_org/brain/"
                deft-extensions '("md" "org" "txt")
                deft-recursive t
                ))

;;(setq browse-url-browser-function 'browse-url-default-browser)
(setq browse-url-browser-function 'w3m-goto-url-new-session)
(setq w3m-default-display-inline-images t)

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

;; use browser depending on url
(setq
 browse-url-browser-function
 '(
  ("wikipedia\\.org" . browse-url-firefox)
  ("github" . browse-url-chromium)
  ("thefreedictionary\\.com" . eww-browse-url)
  ("." . browse-url-default-browser)
  ))

(use-package frog-jump-buffer
  :ensure t
  :defer nil
  :config
)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  )

(use-package eyebrowse
  :ensure t
  :defer nil
  :config
  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode 1)
  )

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
