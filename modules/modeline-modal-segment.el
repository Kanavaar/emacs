;; State alists
(defcustom modeline-meow-state-alist
  '((normal . ("<N>" . font-lock-variable-name-face))
    (insert . ("<I>" . font-lock-string-face))
    (keypad . ("<K>" . font-lock-keyword-face))
    (beacon . ("<B>" . font-lock-type-face))
    (motion . ("<M>" . font-lock-constant-face)))
  "Set the string to the corresponding state of `meow-mode'"
  :group 'modeline
  :type '(alist
          :key-type symbol
          :value-type
          (cons (string :tag "Display Text") (choice :tag "Face" face plist))))

(defcustom modeline-evil-state-alist
  '((normal . ("<N>" . font-lock-variable-name-face))
    (insert . ("<I>" . font-lock-string-face))
    (visual . ("<V>" . font-lock-keyword-face))
    (replace . ("<R>" . font-lock-type-face))
    (motion . ("<M>" . font-lock-constant-face))
    (operator . ("<O>" . font-lock-function-name-face))
    (emacs . ("<E>" . font-lock-builtin-face)))
  "Set the string and corresponding face for any `evil-mode' state.
The `Face' may be either a face symbol or a property list of key-value pairs
 e.g. (:foreground \"red\")."
  :group 'mood-line
  :type '(alist
          :key-type symbol
          :value-type
          (cons (string :tag "Display Text") (choice :tag "Face" face plist))))

;;Segments

;; Meow

(defun modeline-modal-segment--meow ()
  "Display the current `meow-mode' state"
  (when (boundp 'meow--current-state)
    (let ((mode-cons (alist-get
                      meow--current-state
                      modeline-meow-state-alist)))
      (concat (propertize (car mode-cons)
                          'face (cdr mode-cons))
              " "))))

;; Evil

(defun modeline-modal-segment--evil ()
  "Display the current `evil-mode' state"
  (when (boundp 'evil-state)
    (let ((mode-cons (alist-get
                      evil-state
                      modeline-evil-state-alist)))
      (concat (propertize (car mode-cons)
                          'face (cdr mode-cons))
              " "))))

;; Provide package

(provide 'modeline-modal-segment)

;;; modeline-modal-segment.el ends here
