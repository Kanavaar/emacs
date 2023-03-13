;; Variables
(defvar cfg/default-font-size 105)
(defvar cfg/default-variable-font-size 105)
(defvar cfg/font "CaskaydiaCove Nerd Font")
(defvar cfg/variable-font "Cantarell")

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; disable scrollbar
(tool-bar-mode -1) ;; disable tool bar
(tooltip-mode -1) ;; disable tooltip
(set-fringe-mode 10) ;;set fringe to 10
(menu-bar-mode -1) ; disable menu bar
(setq use-short-answers t)
(setq-default tab-width 2)
(global-visual-line-mode t)
(setq select-enable-clipboard t)


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
(set-face-attribute 'variable-pitch nil :font cfg/variable-font :height cfg/default-variable-font-size :weight 'regular)

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

;; Hydra
(use-package hydra
	:ensure t)

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



;; Which-key
(use-package which-key
	:ensure t
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

;; Parens
;; Colored delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (with-eval-after-load 'evil
    (setq sp-show-pair-from-inside t)
    (setq sp-cancel-autoskip-on-backward-movement nil)
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))


  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (smartparens-global-mode t))

;; Clipboard
(use-package simpleclip :config (simpleclip-mode 1))

;; Allows pasting in minibuffer with M-v
(defun cfg/paste-in-minibuffer ()
  (local-set-key (kbd "M-v") 'simpleclip-paste))
(add-hook 'minibuffer-setup-hook 'cfg/paste-in-minibuffer)

;; Editor config
(use-package editorconfig
	:config
	:hook (prog-mode))

;; Scaling text
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-+") 'text-scale-increase)

;; With mouse
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<-C-wheel-down>") 'text-scale-decrease)

;; Looks

;; Authothemer
;;(use-package autothemer
;; :ensure t)

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

;; Usefull programming stuff

;; Magit
(use-package magit
	:ensure t
	:defer t)

;; Org Mode config
(use-package org
	:ensure t
	:config
	(setq org-ellipsis " "
				calendar-week-start-day 1))

(use-package org-bullets
  :hook (org-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; Font scaling
(dolist (face '((org-level-1 . 1.2)
								(org-level-2 . 1.1)
								(org-level-3 . 1.05)
								(org-level-4 . 1.0)
								(org-level-5 . 1.1)
								(org-level-6 . 1.1)
								(org-level-7 . 1.1)
								(org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font cfg/font :weight 'regular :height (cdr face)))

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; org bindings
(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  "H" 'org-shiftleft
  "L" 'org-shiftright
  "<f5>" 'org-ctrl-c-ctrl-c)

(general-def
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "a131602c676b904a5509fff82649a639061bf948a5205327e0f5d1559e04f5ed" "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a" "2ce76d65a813fae8cfee5c207f46f2a256bac69dacbb096051a7a8651aa252b0" "97c99e93cb9167f43837b3c2e8ea1e96f10bf9d1d81ab4e5cc45019e875d9a99" "52779bc51d32d6d9bb91ef680b4a81cd9ef2974c988fba4b8cf8232e63db8919" "a74c85e1fdda49173902f7a1e92db7caf8bffcefc5c634bb37918cb8eb5174ec" "65a245b9dca18a8ede2d9ec672bf8e72d216645b673025fccb6a6dcce6a9b28a" "939c5839f36252387c3faf485cbaed255aa172100f726633da99f0fae9485d5e" "1e1000363c50b8d17efda4c9bebc94d2b9f96be12e3af7a615ba56cd2ef109de" "427cc3110288c2aa479309dc649bd81a88000ded78aad301cc04a4ede196b824" "f5810f881ac9b8c688fc920a626a073edf982164f3d89191dab20544e0ab3da6" "85315f99fa3db1e2dd2401f7fd0a4c02fee627c5d41b54bf514f9c7cf92f8c2f" "fbbe6a6f561cce1a866916a2acf47a9dca34a8183a8966b5e75ec26d8c7976e5" "69ae98c843415e3a37ab5f7e754fe22b11ca2b6973f618432d04982b8c80c804" "47786ea6d1358fab1c0f6123abc668b74557befd572800e8e7b82ccee7c5204e" "bfa8ccaf2d5fa46436ef9922a5fc64f190334dbff222c48654ea16a007857467" "caee937c49d13fbc0226562451a7e2de5d1f28a9b1d42cdddb4b18a71374963d" "6085871fdcd493e27a2d23e5974799b84e3466d26d1aad9e438c81a9e171b1ba" "b3a83bb506cb672c6114c7af931ebc16badb5e75d9a4a16eb5a0717b06b58f8d" "fa18471a6c814d5cc74855e489d16f7841e7fb2d4ccabf32ee9973d13aaca017" "71c328c0536a5b49af5daeac02b3c2ebdd58c4de8eea61c1ed686f363c27b806" "498878c658ab30c9d70c7d695fb1e6bad3f245c66570b9450af5097c8ab8108d" "e704655675d8a39b3edd1ee28b4b4a2693f1a113b3dcf58ce84f5c53dca1d6fb" "048684b923c983cb4cabc9f625aa4612bbbb0337052266825a569459a0271904" "3c8149a057fcb3d909e962a556c05b3dcdb1730ade21a413c91330205b37c4c1" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "d6324e8a397986b032380cf076ed73d2097f3546caa9f554459b10769f55b8e7" "b89430305c7e352c4c9256caf8fcad39d01cdcc78e455b4a025df36e4b8b152f" "34db02351a45841c08369268d5cfa6b5ce49b5449e1c18c893666f57f8970cdd" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" default))
 '(package-selected-packages
	 '(hydra which-key simpleclip org-bullets editorconfig key-chord general magit all-the-icons-dired all-the-icons doom-modeline doom-themes rainbow-delimiters evil-collection evil marginalia consult orderless vertico use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
