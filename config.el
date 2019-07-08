(use-package ox-rst
  :ensure t
  :defer t
  :config
  (require 'ox-rst)
  )

(use-package ox-slack
  :ensure t
  :defer t
  :config
  (require 'ox-slack)
  )

(use-package ox-pandoc
  :ensure t
  :defer t
  :config
  (require 'ox-pandoc)
  )

(setq projectile-sort-order 'recently-active)

;; Change cache file location
(setq projectile-cache-file "~/my_org/emacs_meta/.projectile-cache")

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
ttt          treemacs-no-png-images                 nil
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

(use-package sauron
  :ensure t
  :config
  (require 'sauron)
  (setq sauron-modules '(sauron-org sauron-notifications))
  )

(use-package deft
  :bind ("<f8> d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/my_org/brain/"
                deft-extensions '("md" "org" "txt")
                deft-recursive t
                ))

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

(use-package flyspell
  :hook (
           (prog-mode . flyspell-prog-mode)
           (text-mode . flyspell-mode))
)

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
  "Grep string in journal directory"
  (interactive)
  (let ((specfile (concat "~/my_org/journal/")))
    (let ((helm-ag-command-option (concat helm-ag-command-option "")))
      (helm-ag-this-file specfile))))

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
        ("cheatsheet" . "~/my_projects/ds_cheatsheets/")
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

(message "Loaded scimax customisations")

(setq python-indent-guess-indent-offset nil)

(use-package jupyter
  :ensure t
  :defer t
  :config
  ;(org-babel-load-languages '(jupyter .t))
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "jipython")
                                                       (:kernel . "python3")))
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

(use-package frog-jump-buffer
  :ensure t
  :defer t
  :config
)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
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
