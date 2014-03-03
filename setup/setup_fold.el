;; ====================
;; setup_fold.el
;; ====================

(require 'fold)
(add-hook 'sgml-mode-hook 'fold-mode)
(add-hook 'nxml-mode-hook 'fold-mode)
(add-hook 'css-mode-hook 'fold-mode)
(add-hook 'emacs-lisp-mode-hook 'fold-mode)

(provide 'setup_fold)