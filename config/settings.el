;;; settings.el --- Emacs general settings and appearance

;;------------------------------------------------------------------
;; Appearance
;;------------------------------------------------------------------

;; Theme
(load-theme 'monokai t)
(defvar color-theme-is-global t)
(defvar custom-safe-themes t)

;; Font
(setq vsc-little-font "Source Code Pro-10.5")

;; Keep appearance consistent between frames
(add-to-list 'default-frame-alist (cons 'font vsc-little-font))
(add-to-list 'initial-frame-alist (cons 'font vsc-little-font))

;; Hide everythinga but the menu bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode t)
(scroll-bar-mode -1)

;; Disable startup screen and open scratch buffer showing the emacs version
(setq inhibit-startup-screen t)
(setq initial-scratch-message
      (concat ";; Initialization successful, welcome to "
              (substring (emacs-version) 0 16) ".")) ;; New scratch buffer text

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" (:eval (if (buffer-file-name)
		      (abbreviate-file-name (buffer-file-name))
		    "%b"))))

;;------------------------------------------------------------------
;; Editor
;;------------------------------------------------------------------


(get-time "settings.el")

(provide 'settings)
;;; settings.el ends here
