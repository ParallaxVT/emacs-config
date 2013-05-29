;; ====================
;; sgml mode
;; ====================

(add-hook 'sgml-mode-hook
          '(lambda ()
             (setq sgml-basic-offset 4)))

(provide 'conf_sgml_mode)