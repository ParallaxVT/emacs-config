;; ====================
;; set_hooks.el
;; ====================

(add-hook 'before-save-hook 'whitespace-cleanup nil t)
(add-hook 'after-save-hook 'autocompile)

(provide 'set_hooks)

