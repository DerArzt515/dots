(defvar arzt/editor-theme 'doom-dracula t)

;; You will most likely need to adjust this font size for your system!
(defvar arzt/gdefault-font-size 180)
(defvar arzt/gdefault-variable-font-size 180)

;; Make frame transparency overridable
(defvar arzt/gframe-transparency '(90 . 90))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun arzt/gdisplay-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		     (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'arzt/gdisplay-startup-time)

;; Initialize package sources
 (require 'package)

 (setq package-archives '(("melpa" . "https://melpa.org/packages/")
			  ("org" . "https://orgmode.org/elpa/")
			  ("elpa" . "https://elpa.gnu.org/packages/")))

 (package-initialize)
 (unless package-archive-contents
   (package-refresh-contents))

   ;; Initialize use-package on non-Linux platforms
 (unless (package-installed-p 'use-package)
   (package-install 'use-package))

 (require 'use-package)
 (setq use-package-always-ensure t)

 (use-package auto-package-update
   :custom
   (auto-package-update-interval 7)
   (auto-package-update-prompt-before-update t)
   (auto-package-update-hide-results t)
   :config
   (auto-package-update-maybe)
   (auto-package-update-at-time "09:00"))

 ;; NOTE: If you want to move everything out of the ~/.emacs.d folder
 ;; reliably, set `user-emacs-directory` before loading no-littering!
 ;(setq user-emacs-directory "~/.cache/emacs")

 (use-package no-littering)

 ;; no-littering doesn't set this by default so we must place
 ;; auto save files in the same path as it uses for sessions
 (setq auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar


(use-package doom-themes
  :init (load-theme arzt/editor-theme t))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init 
  (setq doom-modeline-height 10)
  (doom-modeline-mode 1))
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable lin numbers for some modes
(dolist (mode '(org-mode-hook
	     term-mode-hook
	     eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :defer 2
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons-dired
  :requires all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package diff-hl
  :ensure t
  :after magit
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(use-package evil
    :diminish
    :init
	   (setq evil-want-keybinding nil)
	    :config
	    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
	    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

	    (evil-set-initial-state 'messages-buffer-mode 'normal)
	    (evil-set-initial-state 'dashboard-mode 'normal)
	    (evil-mode 1))

  (use-package evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init))

  ;; Highlight text when yanked, popped, deleted or changed with motions. Helps me visually grok what a motion has done.
(use-package evil-goggles
    :requires evil
    :ensure t
    :custom-face
    (evil-goggles-delete-face ((t (:foreground "#620707" :background "#F57373"))))
    (evil-goggles-paste-face ((t (:foreground "#426214" :background "#C3E88D"))))
    (evil-goggles-default-face ((t (:foreground "#002A82" :background "#82AAFF"))))
    :config
    (evil-goggles-mode))

(use-package evil-commentary
:requires evil 
:ensure t
:config (evil-commentary-mode))

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set up the visible bell
  ;;(setq visible-bell t)

  (use-package ivy
	    :diminish 
	    :bind (("C-s" . swiperc)
		   :map ivy-minibuffer-map
		   ("TAB" . ivy-alt-done) 
		   ("C-l" . ivy-alt-done) 
		   ("C-j" . ivy-next-line)
		   ("C-k" . ivy-previous-line)
		   :map ivy-switch-buffer-map
		   ("C-k" . ivy-ivy-previous-line)
		   ("C-l" . ivy-done)
		   ("C-d" . ivy-ivy-switch-buffer-kill)
		   :map ivy-reverse-i-search-map
		   ("C-k" . ivy-previous-line)
		   ("C-d" . ivy-ivy-switch-buffer-kill))
	    :config
	    (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
      ("C-x b" . counsel-ibuffer)
      ("C-x C-f" . counsel-find-file)
      :map minibuffer-local-map
      ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package which-key
	  :init (which-key-mode)
	  :defer 1
  :after evil
	  :diminish which-key-mode
	  :config
	  (setq which-key-idle-delay 0.1
		which-key-side-window-location 'bottom
    which-key-side-window-max-height 0.25
  which-key-show-remaining-keys t
which-key-add-column-padding 5)
	  (which-key-setup-minibuffer))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
    :after lsp-mode
    (add-hook 'lsp-mode .company-mode)
    :bind (:map company-active-map ("<tab>" . company-complete-selection))
    :config
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-backends '((company-files company-keywords company-capf company-yasnippet)
			    (company-abbrev company-dabbrev)))
    :custom
    (company-idle-delay 0.0)
)

(defun reload-conf-file ()
  "This is to reload the main config file for emacs"
  (interactive)
  (load "~/.emacs.d/init.el"))

(use-package general
  :ensure t
  :config

  (general-create-definer arzt/leader-keys
		       :keymaps '(normal insert visual emacs)
		       :prefix "SPC"
		       :global-prefix "C-SPC")
		       )

;; This is stuff for enabling key bindings for major modes akin to
;; what spacemacs and doom do
;; ripped from: https://gist.github.com/progfolio/1c96a67fcec7584b31507ef664de36cc
(general-create-definer global-leader
  :keymaps 'override
  :states '(emacs normal hybrid motion visual operator)
  :prefix "SPC m"
  "" '(:ignore t :which-key "leader"))

(arzt/leader-keys
 "t" '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "choose theme")
 "b" '(:ignore t :which-key "buffers")
 "bb" '(counsel-switch-buffer :which-key "choose buffer")
 "bp" '(previous-buffer :which-key "previous buffer")
 "bn" '(next-buffer :which-key "next buffer")
 "bk" '(kill-buffer :which-key "kill buffer")

 ;; WINDOWS
 "w" '(:ignore t :which-key "windows")
 "ws" '(evil-window-split :which-key "split to above & below")
 "wv" '(evil-window-vsplit :which-key "split to left and right")
 "wj" '(evil-window-down :which-key "go down")
 "wk" '(evil-window-up :which-key "go up")
 "wh" '(evil-window-left :which-key "go left")
 "wl" '(evil-window-right :which-key "go right")
 "q" '(:ignore t :which-key "system")
 "qr" '(reload-conf-file :which-key "reload main conf")
 "qq" '(evil-quit-all :which-key "exit emacs")

 "e" '(:ignore t :which-key "eval")
 "eb" '(eval-buffer :which-key "buffer")

 ;; FILES
 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "find file")

 ;; Search
 "s" '(swiper :which-key "search current file")

;; Comment out
"/" '(evil-commentary :which-key "comment")

 ;; COMMAND
 "SPC" '(counsel-M-x :which-key ":")
 "!" '(shell-command :which-key "Shell CMD")

;; Open
"o" '(:ignore t :which-key "open")
"ot" '(vterm :which-key "vterm"))

(use-package magit
  :after general
  :config
  (arzt/leader-keys
 "g" '(:ignore t :which-key "git")
 "gs" '(magit-status :which-key "git status")
  ))

(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm))

(use-package projectile
  :after general
  :diminish projectile-mode
  :config
  (projectile-mode)
  (arzt/leader-keys
 "p" '(:ignore t :which-key "project")
 "pa" '(projectile-add-known-project :which-key "add project")
 "pf" '(projectile-find-file :which-key "find file in project")
 "pF" '(projectile-find-file-in-known-projects :which-key "find file in other project")
 "pc" '(projectile-compile-project :which-key "compile")
 "pt" '(projectile-test-project :which-key "test project")
 "pr" '(projectile-run-project :which-key "run project")
 "pp" '(projectile-switch-project :which-key "switch project")
 "ps" '(projectile-save-project-buffers :which-key "save buffers")
 "pR" '(projectile-replace-regexp :which-key "replace in project")
 )
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "AWWWWW YIISSSSS"
     dashboard-startup-banner "~/dots/derp.png"
     dashboard-center-content t
     dashbaord-items '((recents . 5)
		       (bookmarks . 5)
		       (projects . 5)
		       (agenda . 5)
		       (registers))
     dashboard-set-heading-icons t
     dashboard-set-file-icons t
     dashboard-set-footer nil
     dashbaord-set-navigator t))

(use-package org
  :custom
 (custom-theme-set-faces
  'user
  `(org-level-8 ((t (,@headline ,@variable-tuple))))
  `(org-level-7 ((t (,@headline ,@variable-tuple))))
  `(org-level-6 ((t (,@headline ,@variable-tuple))))
  `(org-level-5 ((t (,@headline ,@variable-tuple))))
  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" ."src emacs-lisp"))

  (global-leader
:major-modes '(org-mode)
:keymaps '(org-mode-map)

;; Navigation ;;
"n" '(:ignore t :which-key "navigation")
"nn" '(:ignore t :which-key "next")
"nnl" '(org-next-link :which-key "link")
"nni" '(org-next-item :which-key "item")
"nnb" '(org-next-block :which-key "block")
"nnh" '(org-next-visible-heading :which-key "visible heading")

"npn" '(:ignore t :which-key "previous")
"npl" '(org-previous-link :which-key "link")
"npi" '(org-previous-item :which-key "item")
"npb" '(org-previous-block :which-key "block")
"nph" '(org-previous-visible-heading :which-key "visible heading")

;; Table ;;
"t" '(:ignore t :which-key "table")
"tc" '(org-table-create :which-key "insert new table")
"tn" '(:ignore t :which-key "go next")
"tnr" '(org-table-next-row :which-key "next row")
"tnf" '(org-table-next-field :which-key "next field")


;; Table insert;;
"ti" '(:ignore t :which-key "insert")
"tic" '(org-table-insert-column :which-key "insert column left")
"tir" '(org-table-insert-row :which-key "insert row above")
"tih" '(org-table-insert-hline :which-key "insert h-line below")
"td"  '(:ignore t :which-key "delete")
"tdc" '(org-table-delete-column :which-key "delete column")
"tdr" '(evil-delete-whole-line :which-key "delete row")

;; Table Move ;;
"tm" '(:ignore t :which-key "move")
"tmr" '(:ignore t :which-key "move row")
"tmru" '(org-table-move-row-up :which-key "move row up")
"tmrk" '(org-table-move-row-up :which-key "move row up (vim bind)")
"tmrd" '(org-table-move-row-down :which-key "move row down")
"tmrj" '(org-table-move-row-down :which-key "move row down (vim bind)")

"tmc" '(:ignore t :which-key "move column")
"tmcr" '(:ignore t :which-key "move column right")
"tmcl" '(:ignore t :which-key "move column right (vim bind)")
"tmcl" '(:ignore t :which-key "move column left")
"tmch" '(:ignore t :which-key "move column left (vim bind)")

"i" '(:ignore t :which-key "insert")
"il" '(org-insert-link :which-key "link")
"it" '(org-insert-structure-template :which-key "template block")

"a" '(:ignore t :which-key "actions")
"at" '(org-babel-tangle-file :which-key "tangle file")

)


(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
 (setq org-hide-leading-stars nil)
 ;; This line is necessary.
 (setq org-superstar-leading-bullet ?\s)
 ;; If you use Org Indent you also need to add this, otherwise the
 ;; above has no effect while Indent is enabled.
 (setq org-indent-mode-turns-on-hiding-stars nil)
  (org-superstar-headline-bullets-list '("●" "◉" "○" "▶" "▷" "▸")))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "mono" :weight 'bold :height 2)
(dolist (face '((org-level-1 . 1.2)
	     (org-level-2 . 1.1)
	     (org-level-3 . 1.05)
	     (org-level-4 . 1.0)
	     (org-level-5 . 1.1)
	     (org-level-6 . 1.1)
	     (org-level-7 . 1.1)
	     (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "mono" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (python-mode . lsp-deferred)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;(use-package lsp-treemacs
;  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package python-mode
    :ensure nil
    :mode "\\.py\\'"
    :hook (python-mode . lsp-deferred)
    :custom (python-shell-interpereter "python3")
)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			  (require 'lsp-pyright)
			  (lsp))))  ; or lsp-deferred

(fset 'yes-or-no-p 'y-or-n-p)

(use-package exec-path-from-shell)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell which-key vterm use-package rainbow-delimiters python-mode projectile pipenv org-superstar org-roam no-littering magit lsp-ui lsp-pyright lsp-ivy ivy-rich helpful general evil-goggles evil-commentary evil-collection doom-themes doom-modeline diff-hl dashboard dap-mode counsel company auto-package-update all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-default-face ((t (:foreground "#002A82" :background "#82AAFF"))))
 '(evil-goggles-delete-face ((t (:foreground "#620707" :background "#F57373"))))
 '(evil-goggles-paste-face ((t (:foreground "#426214" :background "#C3E88D")))))
