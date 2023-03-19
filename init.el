;; 00 Table of Contents
(occur "^;; [0-9]+")

;; 01 Adding Package manager

(defvar elpaca-installer-version 0.2)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; 02 Basic Options

(use-package emacs
  :elpaca nil
  :init
  (set-charset-priority 'unicode) ;; utf8 in every nook and cranny
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (setq inhibit-startup-message t
        inhibit-splash-screen t)
  (scroll-bar-mode -1) ; disable scrollbar
  (tool-bar-mode -1) ;; disable tool bar
  (tooltip-mode -1) ;; disable tooltip
  (set-fringe-mode 10) ;;set fringe to 10
  (menu-bar-mode -1) ; disable menu bar
  (setq use-short-answers t)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq ring-bell-function 'ignore)
  (global-visual-line-mode t)
  (setq select-enable-clipboard t)
  (recentf-mode 1)

  ;; Backups are annyoing
  (setq make-backups-files nil)
  (setq auto-save-default nil)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq load-prefer-newer t)

  ;; keep backup and save files in a dedicated directory
  (setq backup-directory-alist
        `((".*" . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms
        `((".*" ,(concat user-emacs-directory "backups") t)))

  ;; User config
  (setq user-full-name "Tilman A. Mix")
  (setq user-mail-address "uju8765@gmail.com")

  ;; Custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror)

  (global-display-line-numbers-mode 1) ; enable line numbers
  (setq display-line-numbers-type 'relative)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Font
(defvar cfg/default-font-size 105)
(defvar cfg/default-variable-font-size 130)
(defvar cfg/font "CaskaydiaCove Nerd Font")
(defvar cfg/variable-font "Overpass")

(set-face-attribute 'default nil
										:font cfg/font
										:height cfg/default-font-size
										:weight 'regular)
(set-face-attribute 'fixed-pitch nil
										:font cfg/font
										:height cfg/default-font-size
										:weight 'regular)
(set-face-attribute 'variable-pitch nil
										:font cfg/variable-font
										:height cfg/default-variable-font-size
										:weight 'regular)

;; 03 Completion
(use-package vertico
  :elpaca t
  :config
  (vertico-mode))

(use-package orderless
  :elpaca t
  :config
  (setq completion-styles '(orderless)
        completion-styles-overide '((eglot (styles . (orderless))))))

(use-package consult
  :elpaca t
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

(use-package corfu
  :elpaca t
  :custom
  (corfu-auto t)
  :init
  (corfu-mode 1))

(use-package affe
  :config
  :elpaca t)

;; 04 Parens
(use-package electric
  :init
  (electric-pair-mode 1)
  :elpaca nil)

(use-package rainbow-delimiters
  :elpaca t
  :hook (prog-mode))

;; 05 Modal editing
(use-package evil
  :elpaca t
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
  :elpaca t
  :after evil
  :config
  (evil-collection-init))

;; 06 Hydra
(use-package hydra
	:ensure t)

(elpaca-wait)
;; Org table hydra
(defhydra tilman-hydra-org-table ()
  "
_c_ insert col    _v_ delete col    Move col: _h_, _l_
_r_ insert row    _d_ delete row    Move row: _j_, _k_
_n_ create table  _i_ create hline
_u_ undo
_q_ quit
"
  ("n" org-table-create "create table")
  ("c" org-table-insert-column "insert col")
  ("r" org-table-insert-row "insert row")
  ("v" org-table-delete-column "delete col")
  ("d" org-table-kill-row "delete row")
  ("i" org-table-insert-hline "hline")

  ("u" undo-fu-only-undo "undo")

  ("h" org-table-move-column-left "move col left")
  ("l" org-table-move-column-right "move col right")
  ("k" org-table-move-row-up "move row up")
  ("j" org-table-move-row-down "move row down")

  ("<left>" org-table-previous-field)
  ("<right>" org-table-next-field)
  ("<up>" previous-line)
  ("<down>" org-table-next-row)

  ("q" nil "quit"))

;; 07 Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<-C-wheel-down>") 'text-scale-decrease)

(use-package which-key
	:elpaca t
	:init
	(which-key-mode)
	(which-key-setup-minibuffer)
	:diminish which-key-mode
	:config
	(setq which-key-idle-delay 0.5)
	(setq which-key-prefix-prefix "◉ ")
	(setq which-key-min-display-lines 3
				which-key-max-display-columns nil))

;; Keymaps
(use-package general
  :after evil
  :elpaca t
	:config
	(general-evil-setup t))

(elpaca-wait) ;; I dont know why but it fixed an error

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

 ;; Top level stuff
 "/" '(affe-grep :which-key "ripgrep")
 "SPC" '(execute-extended-command :which-key "M-x")
 "q" '(evil-quit :which-key "quit emacs")
 "." '(find-file :which-key "find files")

 ;; Files
 "f" '(nil :which-key "files")
 "ff" '(find-file :which-key "find files")
 "fF" '(affe-find :which-key "regex find files")
 "fr" '(consult-recent-file :which-key "recent files")
 "fw" '(affe-grep :which-key "ripgrep")
 "fs" '(save-buffer :which-key "save file")
 "fS" '(evil-write-all :which-key "save all buffer")
 "fR" '(rename-buffer :which-key "rename file")

 "d" '(nil :which-key "dired")
 "dd" '(dired :which-key "choose dir")
 "dj" '(dired-jump :which-key "dired in dir of current buffer")

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
 "hpl" 'list-packages
 "hpi" 'package-install
 "hpd" 'package-delete
 "hpa" 'package-autoremove
 
 ;; Toggles
 "t" '(nil :which-key "toggles")
 "tt" '(toggle-truncate-lines :which-key "truncate lines")
 "tv" '(visual-line-mode :which-key "visual line mode")
 "tn" '(display-line-numbers-mode :which-key "display line numbers")
 "th" '(load-theme :which-key "load theme")
 "td" '(disable-theme :which-key "disable theme")

 ;; evaluate
 "e" '(nil :which-key "evaluate")
 "eb" '(eval-buffer :which-key "buffer")
 "el" '(eval-last-sexp :which-key "last expression")
 "ee" '(eval-expression :which-key "expression")
 "er" '(eval-region :which-key "region")

 ;; Git
 "g" '(nil :which-key "git")
 "gs" '(magit-status :which-key "magit")

 ;; Projects
 "p" '(nil :which-key "projects")
 "pp" '(project-switch-project :which-key "switch project")
 "pf" '(project-find-file :which-key "find file")
 "pd" '(project-find-dir :which-key "find dir")
 "pD" '(project-dired :which-key "open dired")
 "pb" '(project-switch-to-buffer :which-key "switch buffer")
 "pe" '(project-eshell :which-key "eshell")
 )

;; Evil Insert bindings
(general-define-key
  :keymaps 'evil-insert-state-map
  (general-chord "jk") 'evil-normal-state
  (general-chord "kj") 'evil-normal-state)
 
(use-package key-chord
  :elpaca t
	:config
	(key-chord-mode t))

;; 08 Clipboard
(use-package simpleclip :config (simpleclip-mode 1))

;; Allows pasting in minibuffer with M-v
(defun cfg/paste-in-minibuffer ()
  (local-set-key (kbd "M-v") 'simpleclip-paste))
(add-hook 'minibuffer-setup-hook 'cfg/paste-in-minibuffer)

;; 09 Appearance
(add-to-list 'custom-theme-load-path (concat (file-name-as-directory user-emacs-directory) "themes"))

(use-package kaolin-themes
	:elpaca t
	:config
	(setq kaolin-themes-bold t
				kaolin-themes-italic t
				kaolin-themes-underline t)
	(setq kaolin-themes-italic-comments t)
  (setq kaolin-ocean-alt-bg t)
  (setq kaolin-galaxy-alt-bg nil))

(use-package doom-themes
  :elpaca t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(elpaca-wait)
;; Theme based on current time
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (if (or (> hour 19) (< hour 7))
      (load-theme 'base16-oxocarbon-dark t) ;; if night (19 to 7)
    (load-theme 'doom-tomorrow-day t))) ;; if day (7 to 19)

(use-package all-the-icons
  :elpaca t)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package minions
  :elpaca t
  :config
  (minions-mode 1))

(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode)

(use-package mood-line
  :elpaca t
  :config
  (mood-line-mode 1))

;; 10 Magit
(use-package magit
	:elpaca t
	:defer t)

;; 11 LSP
(use-package eglot
 :defer t
 :elpaca t)

;; 12 Languages

;; Rust
(use-package rustic
  :elpaca t)

(use-package go-mode
  :elpaca t)

;; 13 Yasnippet

;; 14 Org Mode
(use-package org
	:elpaca t
	:config
	(setq org-ellipsis " ▾"
				calendar-week-start-day 1))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("●" "○" "✸" "✿")
   org-modern-star '( "○" "◈" "◇" "✿")
   org-modern-list '((42 . "◦") (43 . "•") (45 . "•"))
   org-modern-checklist nil
   org-modern-tag t
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil))

;; (use-package org-bullets
;;   :hook (org-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; Font scaling
(with-eval-after-load 'org-faces (dolist (face '((org-level-1 . 1.2)
								(org-level-2 . 1.1)
								(org-level-3 . 1.05)
								(org-level-4 . 1.0)
								(org-level-5 . 1.1)
								(org-level-6 . 1.1)
								(org-level-7 . 1.1)
								(org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font cfg/font :weight 'regular :height (cdr face))))

;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; org bindings
(general-define-key
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  "H" 'org-shiftleft
  "L" 'org-shiftright
  "<f5>" 'org-ctrl-c-ctrl-c)

(general-define-key
	:states '(normal motion)
	:keymaps '(org-mode-map)
	:prefix ","
	"" nil
	"e" '(org-export-dispatch :which-key "export org")
	"s" '(org-schedule :which-key "schedule")
	"d" '(org-deadline :which-key "deadline")

	"1" '(org-toggle-link-display :which-key "toggle link display")
	"2" '(org-toggle-inline-images :which-key "inline images")

	"b" '(nil :which-key "babel")
	"bt" '(org-babel-tangle :which-key "tangle")

	"i" '(nil :which-key "insert")
	"il" '(org-insert-link :which-key "link")
	"l" '(org-insert-link :which-key "insert link")
	"it" '(tilman-hydra-org-table/body :which-key "tables"))

;; 15 Terminal
(use-package vterm
  :requires vterm-module
  :elpaca t)

;; 16 NixOS setup / direnv
(use-package direnv
  :elpaca t
  :config
  (direnv-mode))
