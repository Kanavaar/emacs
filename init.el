;; Variables
(defvar cfg/default-font-size 95)
(defvar cfg/default-variable-font-size 95)
(defvar cfg/font "CaskaydiaCove Nerd Font")
;;(defvar cfg/font "VictorMono Nerd Font Mono")

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; disable scrollbar
(tool-bar-mode -1) ;; disable tool bar
(tooltip-mode -1) ;; disable tooltip
(set-fringe-mode 10) ;;set fringe to 10
(menu-bar-mode -1) ; disable menu bar
(setq use-short-answers t)
(setq-default tab-width 2)
(global-visual-line-mode t)


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
(set-face-attribute 'default nil :font cfg/font :height cfg/default-font-size :weight 'regular)
(set-face-attribute 'fixed-pitch nil :font cfg/font :height cfg/default-font-size :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height cfg/default-variable-font-size :weight 'regular)

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
  (setq evil-want-C-d-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1)
	(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	(evil-global-set-key 'motion "j" 'evil-next-visual-line)
	(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Keymaps
(use-package general
  :ensure t
	:config
	(general-evil-setup t)
  :after evil)

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

 ;; Top level stuff
 "/" '(consult-ripgrep :which-key "ripgrep")
 "SPC" '(execute-extended-command :which-key "M-x")
 "q" '(evil-quit :which-key "quit emacs")
 "." '(find-file :which-key "find files")

 ;; Files
 "f" '(nil :which-key "files")
 "ff" '(find-file :which-key "find files")
 "fw" '(consult-ripgrep :which-key "ripgrep")
 "fs" '(save-buffer :which-key "save file")
 "fS" '(evil-write-all :which-key "save all buffer")
 "fR" '(rename-buffer :which-key "rename file")

 ;; window
 "w" '(nil :which-key "window")
 "wh" '(evil-window-left :which-key "switch to left split")
 "wj" '(evil-window-down :which-key "switch to down split")
 "wk" '(evil-window-up :which-key "switch to up split")
 "wl" '(evil-window-right :which-key "switch to right split")
 "wc" '(evil-window-delete :which-key "close current split")
 "wv" '(evil-window-vsplit :which-key "split window vertical")
 "ws" '(evil-window-split :which-key "split window horizontal")

 ;; Buffer
 "b" '(nil :which-key "buffer")
 "bb" '(consult-buffer :which-key "switch buffer")
 "bd" '(evil-delete-buffer :which-key "close current buffer")

 ;; Help/emacs
 "h" '(nil :which-key "help/emacs")
 
 "hv" '(describe-variable :which-key "des. variable")
 "hb" '(describe-bindings :which-key "des. bindings")
 "hM" '(describe-mode :which-key "des. mode")
 "hf" '(describe-function :which-key "des. func")
 "hF" '(describe-face :which-key "des. face")
 "hk" '(describe-key :which-key "des. key")
 
 "hm" '(nil :which-key "switch mode")
 "hme" '(emacs-lisp-mode :which-key "elisp mode")
 "hmo" '(org-mode :which-key "org mode")
 "hmt" '(text-mode :which-key "text mode")
 
 "hp" '(nil :which-key "packages")
 "hpr" 'package-refresh-contents
 "hpi" 'package-install
 "hpd" 'package-delete
 
 ;; Toggles
 "t" '(nil :which-key "toggles")
 "tt" '(toggle-truncate-lines :which-key "truncate lines")
 "tv" '(visual-line-mode :which-key "visual line mode")
 "tn" '(display-line-numbers-mode :which-key "display line numbers")
 "th" '(load-theme :which-key "load theme")
)

;; Evil Insert bindings
(general-define-key
 :keymaps 'evil-insert-state-map
 (general-chord "jk") 'evil-normal-state
 (general-chord "kj") 'evil-normal-state)
 
(use-package key-chord
  :ensure t
	:config
	(key-chord-mode t))

;; Colored delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

;; Scaling text
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)

;; With mouse
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<-C-wheel-down>") 'text-scale-decrease)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "d6324e8a397986b032380cf076ed73d2097f3546caa9f554459b10769f55b8e7" "b89430305c7e352c4c9256caf8fcad39d01cdcc78e455b4a025df36e4b8b152f" "34db02351a45841c08369268d5cfa6b5ce49b5449e1c18c893666f57f8970cdd" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" default))
 '(package-selected-packages
	 '(key-chord general magit all-the-icons-dired all-the-icons doom-modeline doom-themes rainbow-delimiters evil-collection evil marginalia consult orderless vertico use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
