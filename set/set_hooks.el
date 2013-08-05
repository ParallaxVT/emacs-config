;; ====================
;; set_hooks.el
;; ====================

(add-hook 'before-save-hook 'whitespace-cleanup nil t)
(add-hook 'after-save-hook 'autocompile)

;; Launch eshell when emacs starts
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (concat (getenv "HOME") "/../../work")))
                                    (command-execute 'eshell)
                                    )))

(provide 'set_hooks)
