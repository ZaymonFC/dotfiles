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
(set-face-attribute 'default nil :font "Iosevka SS05" :height 170)

;; -- SANITY ---
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Esc can now quit most dialogues
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual line buffers
  ; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  )

;; Contains various evil key mappings for different modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; -- MAGIT ---
(use-package magit)
  
;; -- UI ---
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

;; Disable line numbers for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode nil))))
