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
(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font" :height 95)

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
  :bind (:map evil-normal-state-map
	      ("SPC f f" . find-file)
              ("SPC f w" . consult-ripgrep)
              ("SPC b b" . consult-buffer)
              ("SPC w" . evil-write)
              ("SPC q" . evil-quit)
              ("SPC SPC" . execute-extended-command))
              ("C-h" . evil-window-left	)
              ("C-j" . evil-window-down	)
              ("C-k" . evil-window-up	)
              ("C-l" . evil-window-right)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Colored delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Looks

;; Nano theme
(use-package nano-theme
  :ensure t)

;; Doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-opera-light t)

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
   '("afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" default))
 '(package-selected-packages
   '(evil-collection nano-theme rainbow-delimiters all-the-icons-dired vertico use-package solo-jazz-theme orderless marginalia evil doom-themes doom-modeline consult all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
