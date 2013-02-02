;; Custom settings
(setq fci-rule-width 2)
(setq fci-rule-color "darkgreen")
(setq fci-rule-column 80)

;; Turn on fci-mode automatically for certain mades

(defun my-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(my-add-to-multiple-hooks
 'fci-mode
 '(text-mode-hook
   emacs-lisp-mode-hook
   nxml-mode-hook
   css-mode-hook
   sh-mode
   fundamental-mode-hook))
