;; package --- Summary

;;; Commentary:

;;; Code:

;; -- HELPERS ---

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;; -- UI SIMPLIFICATION ---

;; Remove the startup message
(setq inhibit-startup-message t)

;; Note: t -> true. nil -> fals.

;; Remove Default UI Elements
(scroll-bar-mode -1) ; No visual scroll bar
(tool-bar-mode -1)   ; Disable the toolbar
;; (tooltip-mode -1) ; No tooltips
(set-fringe-mode 8)  ; Visual breathing room

;; -- TEXT ---
(set-face-attribute 'default nil :font "Iosevka SS05" :height 166)

;; -- SANITY ---
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Esc can now quit most dialogues
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)

;; -- PACKAGE MANAGEMENT ---

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents)) ; Refresh archive if not exists
(unless (package-installed-p 'use-package) (package-install 'use-package)) ; Install `use-package` if not installed

;; Note: functions that end with `-p` are predicates that return `bool | nil`

(require 'use-package)
(setq use-package-always-ensure t)

;; -- ENVIRONMENT AND SHELL ---
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package vterm)

;; -- EMACS LIFECYCLE ---
(use-package restart-emacs)

; --- LINE | COL - NUMBERS --
(column-number-mode)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		vterm-mode
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode nil))))

; --- CORE UI's --
(use-package diminish)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Note 1: This is required for doom-modeline
;; Note 2: The first time you run this on your machine you will need
;; to run the following command to setup fonts on the system
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

(use-package unicode-fonts
   :ensure t
   :config (unicode-fonts-setup))

;; Ivy, Counsel, Helpful.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)))

(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-j" . ivy-next-line)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-j" . ivy-next-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

; --- DISCOVERABILITY ???????????????? --
(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :after (ivy ivy-rich)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package google-this
  :config (google-this-mode 1))

;; --- IVY - POS FRAME --
(defvar ivy-posframe-height)
(defvar ivy-posframe-width)

(defun zan/ivy-posframe-get-size ()
"Set the ivy-posframe size according to the current frame.
This uses the ivy-posframe-height and ivy-posframe-width
variables if they're set.  Otherwise it uses the default
ivy variables or falls back to values I find sensible.

Picks the smaller out of 'columns' or '%frame-width'."
  (let ((height (or ivy-posframe-height (or ivy-height 10)))
        (width (min (or ivy-posframe-width 170) (round (* .60 (frame-width))))))
    (list :height height :width width :min-height height :min-width width)))

(use-package ivy-posframe
  :after (ivy)
  :init
  (setq ivy-posframe-border-width 1)
  (setq ivy-posframe-size-function 'zan/ivy-posframe-get-size)

  (setq ivy-posframe-display-functions-alist
	'((swiper            . ivy-posframe-display-at-frame-bottom-center)
	  (counsel-M-x       . ivy-posframe-display-at-frame-center)
	  (counsel-find-file . ivy-posframe-display-at-frame-center)
	  (counsel-projectile-switch-project . ivy-posframe-display-at-frame-center)
	  ;; Default
	  (t                 . ivy-display-function-fallback)))
  :custom-face (ivy-posframe-border ((t (:background "#51afef"))))
  :config (ivy-posframe-mode 1))

;; --- GENERAL.EL ???? --
(use-package general
  :config
  (general-create-definer zan/leader
    :states '(normal insert emacs)
    :prefix "C-SPC"
    :global-prefix "C-SPC")
  (zan/leader
    "t" '(load-theme :which-key "Load theme")
    "c e" '(lsp-treemacs-errors-list :which-key "List errors (lsp-treemacs)")))

;; -- THEME ---
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

;; -- DASHBOARD ---
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)

  (setq dashboard-startup-banner "~/Projects/dotfiles/emacs-banner.txt")
  (setq dashboard-set-init-info t)

  (setq dashboard-items '((recents  . 6)
                        (projects . 5)
                        (bookmarks . 5)))

  (setq dashboard-footer-messages (read-lines "~/Projects/dotfiles/dashboard-quotes.txt"))
  (setq dashboard-footer-icon (all-the-icons-octicon
			        "law"
				:height 1.1
				:v-adjust 0
				:face 'font-lock-keyword-face))

  :config (dashboard-setup-startup-hook))

;; -- D.R. E.V.I.L. ???? ---
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  :config
  (evil-mode 1)
  ;; C-g is one of emacs escape modes
  ;; Use to escape into normal mode when required
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

;; Contains various evil key mappings for different modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Segregate the internal `kill-ring` from the OSX-Clipboard.
;; This stops evil-mode from yanking into the OS-Clipboard.
(use-package simpleclip
  :after (evil)
  :config (simpleclip-mode 1))

;; -- MAGIT ---
(use-package magit)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode)
  (fringe-helper-define 'git-gutter-fr:added '(center repeated) "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated) "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted '(center repeated) "XXX....."))

;; -- PROJECT MANAGEMENT ???? ---
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package treemacs
  :init (setq treemacs-is-never-other-window t) ; 'C-x o' no longer considers treemacs
  :defer t)

(use-package treemacs-all-the-icons
  :after (treemacs)
  :config (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs))



;; =============================================================================
;; == DEVELOPMENT ===
(use-package flycheck
  :commands flycheck-display-error-messages-unless-error-list
  :init (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
        (setq flycheck-indication-mode 'right-fringe)
  :config (global-flycheck-mode))

;; -- PARENS ---
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :bind (([remap forward-sexp] . sp-forward-slurp-sexp)
	 ([remap backward-sexp] . sp-forward-barf-sexp)
	 ([forward-list] . sp-forward-sexp)
	 ("C-M-b" . sp-backward-sexp)
	 ("M-(" . sp-wrap-round)
	 ("M-[" . sp-wrap-square)
	 ("M-{" . sp-wrap-curly))
  :config
  (smartparens-global-mode 1))

;; -- CLOJURE ---
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  ;; (setenv "PATH" (concat
  ;;                  "/usr/local/bin" path-separator
  ;;                  (getenv "PATH")))
  (lsp-enable-which-key-integration t)

  (dolist (m '(clojure-mode
	       clojurec-mode
	       clojurescript-mode
	       clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :config (global-company-mode))

(use-package cider
  :init (setq cider-repl-buffer-size-limit 250))

;; == / DEVELOPMENT ===
;; =============================================================================

;; Custom. Ignore this.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" default))
 '(package-selected-packages
   '(restart-emacs dashboard vterm google-this simpleclip ivy-posframe all-the-icons-ivy-rich lsp-treemacs git-gutter-fringe flycheck which-key use-package unicode-fonts rainbow-delimiters magit lsp-ui ivy-rich helpful general exec-path-from-shell evil-collection doom-themes doom-modeline diminish counsel-projectile company cider))
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe-border ((t (:background "#51afef")))))

(provide 'init)
;;; init.el ends here
