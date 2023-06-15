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
  (setq use-package-always-defer t)
  (setq elpaca-use-package-by-default t))

(elpaca use-package)

(elpaca-wait)

;; 02 Basic Options

(use-package emacs
  :demand t
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
  (blink-cursor-mode 0)
  (setq use-short-answers t)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  (setq ring-bell-function 'ignore)
  (global-visual-line-mode t)
  (setq select-enable-clipboard t)
  (recentf-mode 1)
  (setq tab-always-indent 'complete)

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
  (setq user-mail-address "tilmanmixyz@proton.me")

  ;; Custom file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file 'noerror)

  ;; Extra load-path
  (add-to-list 'load-path (concat user-emacs-directory "modules"))

  ;; Frame Title
  (setq-default frame-title-format '("Emacs - %b"))

  (global-display-line-numbers-mode 1) ; enable line numbers
  (setq display-line-numbers-type 'relative)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (setq initial-major-mode 'lisp-data-mode)
  (setq initial-scratch-message "")
  
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Font
(defvar cfg/font-spec-size 14)
(defvar cfg/variable-font-spec-size 14)
(defvar cfg/font "CaskaydiaCove Nerd Font")
(defvar cfg/variable-font "Overpass")

(set-frame-font (font-spec :family cfg/font :size cfg/font-spec-size))
(add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font"))

;; 03 Completion
(use-package vertico
  :demand t
  :elpaca t
  :config
  (vertico-mode))

(use-package orderless
  :elpaca t
  :demand t
  :after vertico
  :config
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :defer t
  :elpaca t)

(use-package marginalia
  :demand t
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
  :defer t
  :elpaca t
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode 1))

(use-package cape
  :defer 10
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  :elpaca t)

(use-package affe
  :commands (affe-grep affe-find)
  :config
  :elpaca t)

;; 04 Parens
(use-package electric
  :defer t
  :init
  (electric-pair-mode 1)
  :elpaca nil)

(show-paren-mode)
(defun show-paren--locate-near-paren-ad ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((before (show-paren--categorize-paren (point))))
    (when (or
           (eq (car before) 1)
           (eq (car before) -1))
      before)))

(advice-add 'show-paren--locate-near-paren
            :override #'show-paren--locate-near-paren-ad)

(use-package rainbow-delimiters
  :elpaca t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 05 Modal editing
(use-package meow
  :demand t
  :init
  (defun meow-setup ()
    (global-set-key (kbd "C-h C-f") nil)
    (global-set-key (kbd "C-h C-m") nil)
    (global-set-key (kbd "C-h m") nil)
    (global-set-key (kbd "C-M-s") 'magit-status)
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     ;; Top level stuff
     '("/" . comment-line)
     '("SPC" . execute-extended-command)
     '("q" . kill-emacs)
     '("." . find-file)
     ;; File
     '("ff" . find-file)
     '("fF" . affe-find)
     '("fr" . consult-recent-file)
     '("fw" . affe-grep)
     '("fs" . save-buffer)
     '("fR" . rename-buffer)
     ;; Dired
     '("dd" . dired)
     '("dj" . dired-jump)
     ;; Window
     '("wv" . vsplit-follow)
     '("ws" . split-follow)
     '("wc" . delete-window)
     '("wh" . windmove-left)
     '("wj" . windmove-down)
     '("wk" . windmove-up)
     '("wl" . windmove-right)
     ;; Buffer
     '("b" . nil)
     '("bb" . consult-buffer)
     '("bd" . kill-this-buffer)
     ;; Help/Emacs
     '("h" . nil)
     '("hv" . describe-variable)
     '("hb" . describe-bindings)
     '("hf" . describe-function)
     '("hF" . describe-face)
     '("hk" . describe-key)
     '("hi" . info)
     ;; Mode switching
     '("hm" . nil)
     '("hme" . emacs-lisp-mode)
     '("hmo" . org-mode)
     '("hmt" . text-mode)
     '("hmr" . rustic-mode)
     '("hml" . eglot)
     ;; Toggling
     '("th" . load-theme)
     '("td" . disable-theme)
     '("tn" . display-line-numbers-mode)
     '("tv" . visual-line-mode)
     '("tt" . toggle-truncate-lines)
     ;; Evaluate
     '("ee" . eval-expression)
     '("eb" . eval-buffer)
     '("el" . eval-last-sexp)
     '("er" . eval-region)
     '("ed" . eval-defun)
     ;; Git
     '("g" . nil)
     '("gs" . magit-status)
     ;; Projects
     '("pp" . project-switch-project)
     '("pf" . project-find-file)
     '("pd" . project-find-dir)
     '("pb" . project-switch-to-buffer)
     '("pe" . project-eshell)
     ;; Org mode
     '("o" . nil)
     '("ol" . org-insert-link)
     '("ot" . org-tables)
     '("oe" . org-export-dispatch)
     '("od" . org-deadline)
     '("os" . org-schedule)
     '("obt" . org-babel-tangle)
     '("of" . org-open-at-point)
     ;; cheatsheet
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     ;; '("," . meow-inner-of-thing)
     ;; '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . avy-goto-char)
     '("F" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("/" . consult-line)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode)
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(Info-mode . normal))
  :elpaca t)

(use-package transient
  :demand t
  :config
  (transient-define-prefix org-tables ()
    "A `transient' interface for managing org tables"
    ["Manage tables"
     ("n" "New Table" org-table-create)
     ("c" "insert col" org-table-insert-column)
     ("r" "insert row" org-table-insert-row)
     ("v" "delete col" org-table-delete-column)
     ("d" "delete row" org-table-kill-row)
     ("u" "undo" undo-fu-only-undo)
     ("h" "move col left" org-table-move-column-left)
     ("l" "move col right" org-table-move-column-right)
     ("j" "move row down" org-table-move-row-down)
     ("k" "move row up" org-table-move-row-up)
     ("<left>" "move to prev field" org-table-previous-field)
     ("<right>" "move to next field" org-table-next-field)
     ("<up>" "move to upper row" previous-line)
     ("<down>" "move to lower row" org-table-next-row)])
  :elpaca t)

;; 07 Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<-C-wheel-down>") 'text-scale-decrease)
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode)
  (global-set-key (kbd "<wheel-up>") 'pixel-scroll-up)
  (global-set-key (kbd "<wheel-down>") 'pixel-scroll-down))

;; Keymaps

(defun vsplit-follow ()
  (interactive)
  (split-window-right)
  (other-window 1))
(defun split-follow ()
  (interactive)
  (split-window-below)
  (other-window 1))

(use-package key-chord
  :after meow
  :elpaca t
	:init
  (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
  (key-chord-define meow-insert-state-keymap "kj" 'meow-insert-exit)
  :config
	(key-chord-mode t))

;; 08 Clipboard
(use-package simpleclip
  :defer t
  :config
  (simpleclip-mode 1)
  :elpaca t)

;; Allows pasting in minibuffer with M-v
(defun cfg/paste-in-minibuffer ()
  (local-set-key (kbd "M-v") 'simpleclip-paste))
(add-hook 'minibuffer-setup-hook 'cfg/paste-in-minibuffer)

;; 09 Appearance
(add-to-list 'custom-theme-load-path (concat (file-name-as-directory user-emacs-directory) "themes"))

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-region '(bg-only)
      modus-themes-paren-match '(bold intense underline)
      modus-themes-fringes 'subtle
      modus-themes-syntax '(yellow-comments))

;; Theme based on current time
(let ((hour (string-to-number (substring (current-time-string) 11 13))))
  (if (or (> hour 19) (< hour 7) (equal window-system nil))
      (load-theme 'nightfox t) ;; if night (19 to 7)
    (load-theme 'modus-operandi t))) ;; if day (7 to 19)

(use-package all-the-icons
  :defer t
  :elpaca t)

(use-package all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package minions
  :elpaca t
  :config
  (minions-mode 1))

(setq display-time-default-load-average nil
      display-time-24hr-format t)
(display-time-mode)

(require 'modeline)
(modeline-mode 1)
;; (use-package mood-line
;;   :elpaca t
;;   :config
;;   (mood-line-mode 1))
;; (use-package telephone-line
;;   :config
;;   (setq telephone-line-primary-left-separator 'telephone-line-sin-left
;;         telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
;;         telephone-line-primary-right-separator 'telephone-line-sin-right
;;         telephone-line-secondary-right-separator 'telephone-line-sin-hollow-right)
;;   (setq telephone-line-height 20)
;;   (setq telephone-line-evil-use-short-tag t)
;;   (telephone-line-defsegment* telephone-line-simpler-major-mode-segment ()
;;     (concat "["
;;             (if (listp mode-name)
;;                 (car mode-name)
;;               mode-name)
;;             "]"))
;;   (telephone-line-defsegment* telephone-line-simple-pos-segment ()
;;     (concat "%c : " "%l/" (number-to-string (count-lines (point-min) (point-max)))))
;;   (setq telephone-line-lhs
;;         '((nil . (telephone-line-meow-tag-segment
;;                   telephone-line-buffer-modified-segment))
;;           (accent . (telephone-line-projectile-buffer-segment))
;;           (nil . (telephone-line-simple-pos-segment)))
;;         telephone-line-rhs
;;         '((nil . (telephone-line-simpler-major-mode-segment))
;;           (accent . (telephone-line-misc-info-segment))))

;;   (telephone-line-mode 1)
;;   :elpaca t)

;; 10 Magit
(use-package magit
	:elpaca t
	:defer t)

;; 11 LSP
(use-package eglot
  :hook (rustic-mode . eglot-ensure)
  :config
  (setq completion-category-overrides '((eglot (styles orderless))))
  :elpaca nil)

;; 12 Languages

;; Rust
(use-package rustic
  :defer t
  :init
  (setq rustic-lsp-client 'eglot
        rustic-rls-pkg 'eglot)
  :elpaca t)

;; Go
(use-package go-mode
  :defer t
  :elpaca t)

;; Zig
(use-package zig-mode
  :defer t
  :elpaca t)

;; Haskell
(use-package haskell-mode
  :defer t
  :elpaca t)

;; Nix/NixOS For shell and flakes
(use-package nix-mode
  :defer t
  :elpaca t)

;; 13 Tree-sitter and other highlighting
(use-package tree-sitter
  :hook ((rustic-mode . tree-sitter-mode)
         (go-mode . tree-sitter-mode)
         (haskell-mode . tree-sitter-mode)
         (nix-mode . tree-sitter-mode)
         (zig-mode . tree-sitter-mode)
         (tree-sitter-mode . tree-sitter-hl-mode))
  :elpaca t)

(use-package tree-sitter-langs
  :defer t
  :elpaca t)

;; hl-todo
(use-package hl-todo
  :defer t
  :elpaca t
  :hook (prog-mode . hl-todo-mode))

;; 14 Flycheck
(use-package flycheck
  :defer t
  :elpaca t)

;; 15 Yasnippet

;; 16 Org Mode
(use-package org
  :defer t
	:elpaca t
  :hook (org-mode . org-disable-keys)
	:config
	(setq org-ellipsis " ▾"
				calendar-week-start-day 1))

(use-package org-modern
  :defer t
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("●" "○" "✸" "✿")
   org-modern-star '( "○" "◈" "◇" "✿")
   org-modern-list '((42 . "•") (43 . "•") (45 . "•"))
   org-modern-checklist nil
   org-modern-tag t
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil))

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

;; org bindings
(defun org-disable-keys ()
  (define-key org-mode-map (kbd "<return>") nil))

;; LaTeX Export Options
;; Setting default compiler, this is used for fonts, cuz they bad in default latex
(setq org-latex-compiler "lualatex")

;; Enable Code Highlighting
(setq org-latex-listings 't)

;; 17 Terminal
(use-package vterm
  :defer t
  :requires vterm-module
  :elpaca t)

;; 18 NixOS setup / direnv
(use-package direnv
  :elpaca t
  :config
  (direnv-mode))

;; 19 Rainbow Mode
(use-package rainbow-mode
  :defer t
  :commands (rainbow-mode)
  :elpaca t)

;; 20 Undo
(use-package undo-fu
  :defer t
  :elpaca t)

(use-package vundo
  :defer t
  :elpaca t)

;; 21 Avy
(use-package avy
  :defer t
  :elpaca t)

;; 22 Indentation
(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  :elpaca t)

;; 23 Dashboard
(use-package dashboard
  :demand t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (setq dashboard-startup-banner (concat user-emacs-directory "assets/ferris.txt"))
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-items '())
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-messages '("Zwei Dinge sind unendlich, das Universum und die menschliche Dummheit, aber bei dem Universum bin ich mir noch nicht ganz sicher." "Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden." "There are 2 hard problems in computer science: cache invalidation, naming things, and off-by-1 errors." "There are 10 types of people in this world: those who understand binary, and those who don't."))
  (dashboard-open)
  :elpaca t)


