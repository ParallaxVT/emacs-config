;; ====================
;; setup_hooks.el
;; ====================

(add-hook 'before-save-hook 'whitespace-cleanup nil t)
(add-hook 'after-save-hook 'autocompile)

;; Launch eshell when emacs starts
;; (add-hook 'emacs-startup-hook #'(lambda ()
;; (let ((default-directory (concat (getenv "HOME") "/../../work")))
;; (command-execute 'eshell)
;; )))

(add-hook 'sgml-hook
          '(lambda ()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq sgml-basic-offset 4)
             (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
             (setq indent-tabs-mode nil)))

(add-hook 'html-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq sgml-basic-offset 4)
             (setq indent-tabs-mode nil)))

(add-hook 'xml-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq sgml-basic-offset 4)
             (setq indent-tabs-mode nil)))

(provide 'setup_hooks)
