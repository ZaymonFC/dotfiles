;;; Code:

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

;; -- ENVIRONMENT ---
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; -- PACKAGE SETUP ---
(use-package diminish)

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

(use-package ivy-rich
  :init (ivy-rich-mode 1))

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

;; --- GENERAL.EL 🎖 --
(use-package general
  :config
  (general-create-definer zan/leader
    :states '(normal insert emacs)
    :prefix "C-SPC"
    :global-prefix "C-SPC")
  (zan/leader
    "t" '(load-theme :whick-key "Load theme")
    "c e" '(lsp-treemacs-errors-list :which-key "List errors (lsp-treemacs)")))

;; - Doom Mode Line
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
   :config
    (unicode-fonts-setup))

;; -- THEME ---
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

;; -- D.R. E.V.I.L. 😈 ---
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

;; -- MAGIT ---
(use-package magit)

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode)
  (fringe-helper-define 'git-gutter-fr:added '(center repeated) "XXXXXX..")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated) "XXXXXX..")
  (fringe-helper-define 'git-gutter-fr:deleted '(center repeated) "XXXXXX.."))

;; -- PROJECT MANAGEMENT 🔫 ---
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

;; -- UI ---
(column-number-mode)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode nil))))


;; =============================================================================
;; == DEVELOPMENT ===

(use-package flycheck
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
  :commands (lsp lsp-deferred)
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
   '(lsp-treemacs git-gutter-fringe flycheck which-key use-package unicode-fonts rainbow-delimiters magit lsp-ui ivy-rich helpful general exec-path-from-shell evil-collection doom-themes doom-modeline diminish counsel-projectile company cider))
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
