(eval-when-compile)

;; Constant definition
(defconst modeline-chars-alist
  '((:buffer-modified . ?*)
    (:buffer-read-only . ?#)))

;; Group Defenition
(defgroup modeline nil
  "A minimal mode-line"
  :group 'mode-line)

(defgroup modeline-faces nil
  "Faces for `modeline'"
  :group 'modeline
  :group 'faces)

;; Faces definition
(defface modeline-buffer-name
  '((t (:inherit mode-line-buffer-id)))
  "Face for the buffer name"
  :group 'modeline-faces)

(defface modeline-buffer-status-modified
  '((t (:inherit error :weight normal)))
  "Face for the modified status indicator of the buffer"
  :group 'modeline-faces)

(defface modeline-buffer-status-read-only
  '((t (:inherit shadow :weight normal)))
  "Face for displaying read only status indicator if buffer is read only."
  :group 'modeline-faces)

(defface modeline-major-mode
  '((t (:inherit bold)))
  "Face used for displaying the major mode."
  :group 'modeline-faces)

(defface modeline-unimportant
  '((t (:inherit shadow :weight normal)))
  "Face used for not as important parts of the modeline."
  :group 'modeline-faces)

;; Helper functions
(defun modeline--format (left right)
  "Format a mode line with a `LEFT' and `RIGHT' justified list of elements.
The mode line should fit the `window-width' with space between the lists."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right
                                                       (- 0 right-margin)
                                                       ,reserve))))
            right)))

(defun modeline--get-char (char)
  "Return the mathcing character from `modeline-chars-alist'"
  (char-to-string (alist-get char
                             modeline-chars-alist)))

;; Segments

(defun modeline-buffer-status-segment ()
  "Return an indicator for the currnts buffers status"
  (concat (if (buffer-file-name (buffer-base-buffer))
              (cond
               ((buffer-modified-p)
                (propertize (modeline--get-char :buffer-modified)
                            'face 'modeline-buffer-status-modified))
               ((buffer-read-only)
                (propertize (modeline--get-char :buffer-read-only)
                            'face 'modeline-buffer-status-read-only))
               " "))
          " "))

(defun modeline-buffer-name-segment ()
  "A segment which shows the name if the current buffer."
  (propertize "%b "
              'face 'modeline-buffer-name))

(defun modeline-cursor-position-segment ()
  "A segment which shows the cursors current position."
  (concat
   "%l:%c"
   (propertize " %p%%  "
               'face 'modeline-unimportant)))

(defun modeline-major-mode-segment ()
  "A segment which displays the current major mode."
  (concat (propertize (substring-no-properties (format-mode-line mode-name))
                      'face 'modeline-major-mode)
          " "))

(defun modeline-misc-info-segment ()
  "A segment which didplays the content of `mode-line-misc-info'."
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (concat (propertize (string-trim misc-info)
                          'face 'modeline-unimportant)
              " "))))

;; Minor mode definition
(defun modeline--activate ()
  (setq modeline--default-mode-line mode-line-format)
  (setq-default mode-line-format
                '((:eval
                   (modeline--format
                    ;; Left
                    (format-mode-line
                     '(" "
                       (:eval (modeline-buffer-status-segment))
                       (:eval (modeline-buffer-name-segment))
                       (:eval (modeline-cursor-position-segment))))

                    ;; Right
                    (format-mode-line
                     '(" "
                       (:eval (modeline-major-mode-segment))
                       (:eval (modeline-misc-info-segment))
                       " ")))))))

(defun modeline--deactivate ()
  (setq-default mode-line-format modeline--default-mode-line))

(define-minor-mode modeline-mode
  "Toggle modeline on or off"
  :group 'modeline
  :global t
  :lighter nil
  (if modeline-mode
      (modeline--activate)
    (modeline--deactivate)))

;; Provide modeline
(provide 'modeline)

;;; modeline.el ends here