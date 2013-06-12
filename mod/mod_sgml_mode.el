;; ====================
;; mod_sgml_mode.el
;; ====================

(add-hook 'sgml-mode-hook
          '(lambda ()
             (setq sgml-basic-offset 4)))

(provide 'mod_sgml_mode)