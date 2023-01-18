(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; disable scrollbar
(tool-bar-mode -1) ;; disable tool bar
(tooltip-mode -1) ;; disable tooltip
(set-fringe-mode 10) ;;set fringe to 10
;; (menu-bar-mode -1) ; disable menu bar

(global-display-line-numbers-mode 1) ; enable line numbers
(setq display-line-numbers-type 'relative)

(load-theme 'wombat)
;;; load custom theme (add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

;; Font Stuff
(set-face-attribute 'default nil :font "Iosevka" :height 110)


;; Packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non linux
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages '(doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'use-package)
(setq use-package-always-ensured t)

; Autothemer
(use-package autothemer)

;; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

; Ivy completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

; Counsel
;(use-package counsel
;  :bind (("M-x" . counsel-M-x)
;	 ("C-x b" . counsel-ibuffer)
;	 ("C-x C-f" . counsel-find-file)
;	 :map minibuffer-local-map
;	 ("C-r" . counsel-minibuffer-history))
;  :config
;  (setq ivy-initial-inputs-alist nil))

;; Keymap stuff
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)