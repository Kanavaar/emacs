;; MIT License

;; Copyright (c) 2023 Tilman Mix

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code

(deftheme nightfox
  "A customized version of the nightfox theme for neovim (https://github.com/EdenEast/nightfox.nvim)")

(let* ((class '((class color) (min-colors 256)))
       (mode-line-padding 5)
       (true-colors-available-p (or (daemonp)
                                    (display-graphic-p)
                                    (>= (tty-display-color-cells) 1677216)))

       ;; Backgrounds
       (bg (if true-colors-available-p "#080808" "grey2")) ;; Default bf
       (bg1 (if true-colors-available-p "#131a24" "grey4")) ;; Modeline
       (bg3 (if true-colors-available-p "#29394f" "grey16")) ;; hl-line-mode, active line number

       ;; Foregrounds
       (fg0 (if true-colors-available-p "#d6d6d7" "grey90"))
       (fg (if true-colors-available-p "#cdcecf" "grey80")) ;; Default fg
       (fg2 (if true-colors-available-p "#aeafb0" "grey69")) ;; modeline
       (fg3 (if true-colors-available-p "#71839b" "LightSkyBlue4")) ;; inactive line number

       Selection
       (sel (if true-colors-available-p "#2b3b51" "grey16")) ;; region
       (sel1 (if true-colors-available-p "#3c5372" "LightSkyBlue4")) ;; isearch inactive

       ;; Colors
       (black (if true-colors-available-p "#393b44" "grey22"))
       (red (if true-colors-available-p "#c94f6d" "tomato3")) ;; error
       (green (if true-colors-available-p "#81b29a" "DarkSeaGreen4")) ;; success strings
       (yellow (if true-colors-available-p "#dbc074" "goldenrod")) ;; warning
       (blue (if true-colors-available-p "#719cd6" "SkyBlue4"))
       (magenta (if true-colors-available-p "#9d79d6" "MediumPurple4"))
       (cyan (if true-colors-available-p "#63cdcf" "DarkSlateGray3"))
       (white (if true-colors-available-p "#dfdfe0" "grey88"))
       (orange (if true-colors-available-p "#f4a261" "orange"))
       (pink (if true-colors-available-p "#d67ad2" "MediumOrchid"))
       )
  (custom-theme-set-faces
   'nightfox

   ;; Core Emacs faces
   ;; Default
   `(default ((,class (:background ,bg :foreground ,fg))))

   ;; Emacs faces
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,yellow))))
   `(success ((,class (:foreground ,green))))
   `(fringe ((,class (:inherit default))))
   `(region ((,class (:background ,sel :foreground nil :distant-foreground ,fg))))
   `(highlight ((,class (:background ,sel :slant italic :foreground ,yellow))))
   `(lazy-highlight ((,class (:inherit highlight))))
   `(cursor ((,class (:background ,fg))))
   `(shadow ((,class (:foreground ,fg0))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(tooltip ((,class (:background ,bg1 :foreground ,fg))))
   `(secondary-selection ((,class (:background ,sel1))))
   `(fill-column-indicator ((,class (:background ,bg3))))
   `(match ((,class (foreground ,green :weight bold))))
   `(trailing-whitespace ((,class (:background ,red))))
   `(nobreak-space ((,class (:inherit default :underline t))))
   `(nobreak-hyphen ((,class (:inherit nobreak-space))))
   `(vertical-border ((,class (:background ,bg1 :foreground ,bg1))))
   `(link ((,class ((:foreground ,blue :underline t :weight bold)))))
   `(link-visited ((,class (:inherit link :foreground ,magenta))))
   `(escape-glyph ((,class (:foreground ,pink))))
   `(homoglyph ((,class (:inherit escape-glyph))))
   `(widget-single-line-field ((,class (:background ,fg :foreground ,bg))))
   `(widget-field ((,class (:inherit widget-single-line-field :extend t))))

   ;; Font lock
   `(font-lock-builtin-face ((,class (:foreground ,red))))
   `(font-lock-doc-face ((,class (:foreground ,fg2 :weight bold))))
   `(font-lock-comment-face ((,class :foreground ,fg3 :slant italic :weight bold)))
   `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((,class (:foreground ,orange))))
   `(font-lock-variable-name-face ((,class (:foreground ,white))))
   `(font-lock-variable-use-face ((,class (:inherit font-lock-variable-name-face))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta :slant italic))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-warning-face ((,class (:inherit warning))))
   `(font-lock-preprocessor-face ((,class (:inherit font-lock-builtin-face))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg2))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green))))
   `(font-lock-regexp-grouping-construct ((,class (:inherit font-lock-regexp-grouping-backslash))))

   ;; mode-/header-line
   `(mode-line ((,class :background ,bg3 :foreground ,fg2 :box (:line-width ,mode-line-padding :color ,bg3))))
   `(mode-line-inactive ((,class (:inherit mode-line :background ,bg1 :box (:line-width ,mode-line-padding :color ,bg1)))))
   `(mode-line-emphasis ((,class (:foreground ,blue))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground ,fg0))))
   `(header-line ((,class (:inherit mode-line-inactive))))

   ;; Internal/Build-in packages

   ;; ansi/terminal colors
   `(ansi-color-black ((,class (:foreground ,black :background ,black))))
   `(ansi-color-red ((,class (:foreground ,red :background ,red))))
   `(ansi-color-green ((,class (:foreground ,green :background ,green))))
   `(ansi-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(ansi-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(ansi-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(ansi-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(ansi-color-white ((,class (:foreground ,white :background ,white))))
   `(ansi-color-bright-black ((,class (:foreground "#575860" :background "#575860"))))
   `(ansi-color-bright-red ((,class (:foreground "#d16983" :background "#d16983"))))
   `(ansi-color-bright-green ((,class (:foreground "#8ebaa4" :background "#8ebaa4"))))
   `(ansi-color-bright-yellow ((,class (:foreground "#e0c989" :background "#e0c989"))))
   `(ansi-color-bright-blue ((,class (:foreground "#86abdc" :background "#86abdc"))))
   `(ansi-color-bright-magenta ((,class (:foreground "#baa1e2" :background "#baa1e2"))))
   `(ansi-color-bright-cyan ((,class (:foreground "#7ad5d6" :background "#7ad5d6"))))
   `(ansi-color-bright-white ((,class (:foreground "#e4e4e5" :background "#e4e4e5"))))

   ))

;; The providing

(provide-theme 'nightfox)

;;; nightfox-theme.el ends here
