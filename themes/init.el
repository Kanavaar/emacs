;; Variables
(defvar tilman/default-font-size 95)
(defvar tilman/default-variable-font-size 95)
(defvar tilman/font "CaskaydiaCove Nerd Font")
;;(defvar tilman/font "VictorMono Nerd Font Mono")

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; disable scrollbar
(tool-bar-mode -1) ;; disable tool bar
(tooltip-mode -1) ;; disable tooltip
(set-fringe-mode 10) ;;set fringe to 10
(menu-bar-mode -1) ; disable menu bar

(global-display-line-numbers-mode 1) ; enable line numbers
(setq display-line-numbers-type 'relative)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Keymap stuff
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Themes
;; (load-theme 'wombat)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Font
(set-face-attribute 'default nil :font tilman/font :height tilman/default-font-size :weight 'regular)
(set-face-attribute 'fixed-pitch nil :font tilman/font :height tilman/default-font-size :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height tilman/default-variable-font-size :weight 'regular)

;; Plugins
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

;; Completion

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package consult
  :ensure t
  :bind (
	 ("C-x b" . consult-buffer)
	 ))

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Modal editing
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-normal-state-map (kbd "SPC f f") 'find-file)
  (define-key evil-normal-state-map (kbd "SPC f w") 'consult-ripgrep)
  (define-key evil-normal-state-map (kbd "SPC b b") 'consult-buffer)
  (define-key evil-normal-state-map (kbd "SPC b d") 'evil-delete-buffer)
  (define-key evil-normal-state-map (kbd "SPC w") 'evil-write)
  (define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)
  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (global-set-key (kbd "C-l") 'evil-window-right)
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Keymaps
(use-package general
  :ensure t
  :after evil)

(use-package key-chord
  :ensure t)

;; Colored delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

;; Looks

;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-day t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-height 20)
  :init (doom-modeline-mode 1))

;; Font
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Keybindings
;;(define-key evil-normal-state-map (kbd "SPC f f") 'find-file)
;;(define-key evil-normal-state-map (kbd "SPC f w") 'consult-ripgrep)
;;(define-key evil-normal-state-map (kbd "SPC b b") 'consult-buffer)
;;(define-key evil-normal-state-map (kbd "SPC w") 'evil-write)
;;(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
;;(define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)
;;(global-set-key (kbd "C-h") 'evil-window-left)
;;(global-set-key (kbd "C-j") 'evil-window-down)
;;(global-set-key (kbd "C-k") 'evil-window-up)
;;(global-set-key (kbd "C-l") 'evil-window-right)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "d6324e8a397986b032380cf076ed73d2097f3546caa9f554459b10769f55b8e7" "b89430305c7e352c4c9256caf8fcad39d01cdcc78e455b4a025df36e4b8b152f" "34db02351a45841c08369268d5cfa6b5ce49b5449e1c18c893666f57f8970cdd" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" default))
 '(package-selected-packages
   '(key-chord general magit all-the-icons-dired all-the-icons doom-modeline doom-themes rainbow-delimiters evil-collection evil marginalia consult orderless vertico use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
