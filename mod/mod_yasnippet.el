;; ====================
;; mod_yasnippet.el
;; ====================

(require 'yasnippet)

(add-hook 'html-mode-hook 'yas/minor-mode-on)
(add-hook 'css-mode-hook 'yas/minor-mode-on)
(add-hook 'php-mode-hook 'yas/minor-mode-on)
(add-hook 'nxml-mode-hook 'yas/minor-mode-on)

(setq yas/root-directory "~/.emacs/snippets/")

(yas/load-directory yas/root-directory)

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

(provide 'mod_yasnippet)