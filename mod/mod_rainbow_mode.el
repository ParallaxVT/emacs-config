;; ====================
;; mod_rainbow_mode.el
;; ====================

(add-hook 'html-mode-hook
          '(lambda ()
             (require 'rainbow-mode)
             (rainbow-mode 1)
             ))

(add-hook 'css-mode-hook
          '(lambda ()
             (require 'rainbow-mode)
             (rainbow-mode 1)
             ))

(provide 'mod_rainbow_mode)