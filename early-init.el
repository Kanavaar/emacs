;; Set Garbage Collection threshold for faster startup
(setq gc-cons-threshold (* 50 100 1000))

;; Native Comp settings
(when (featurep 'native-compile)
  ;; Silence annyoing warning
  (setq native-comp-async-report-warnings-error nil))
;; Set correct Natice comp cache path
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Disable package.el for elpaca to function
(setq package-enable-at-startup nil)
