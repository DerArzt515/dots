(defvar arzt/editor-theme 'doom-dark+)

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

(use-package evil
             :config
             (evil-mode 1)
             (evil-global-set-key 'motion "j" 'evil-next-visual-line)
             (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

             (evil-set-initial-state 'messages-buffer-mode 'normal)
             (evil-set-initial-state 'dashboard-mode 'normal)
             )

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package doom-themes
  :init (load-theme arzt/editor-theme t))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init 
  (setq doom-modeline-height 10)
  (doom-modeline-mode 1))

;; Set up the visible bell
(setq visible-bell t)

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
  :defer 1
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

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
  

(use-package magit
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit rainbow-delimiters doom-mode-line counsel no-littering auto-package-update use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
