;; ====================
;; mod_eshell.el
;; ====================

(setq eshell-ask-to-save-history (quote always))
(setq eshell-history-size 1000)
(setq eshell-ask-to-save-history 'always)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-ls-dired-initial-args (quote ("-h")))
(setq eshell-ls-initial-args "-h")
(setq eshell-ls-use-in-dired t)
;; scroll to the bottom
(setq eshell-output-filter-functions (quote (eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom)))
(setq eshell-scroll-show-maximum-output t)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-glob-include-dot-dot nil)

(provide 'mod_eshell)