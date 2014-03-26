;; ====================
;; setup_autocomplete.el
;; ====================

(require 'auto-complete-config)

(when (require 'auto-complete-config nil 'noerror) ;; don't break if not installed
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/misc/ac-dict")
  (setq ac-comphist-file  "~/.emacs.d/misc/ac-comphist.dat")
  (ac-config-default)

  (setq ac-stop-words (quote ("/" "//" "/*" "//*" "///" "////"))
        ac-ignore-case t ;; ignore case
        ac-use-fuzzy t ;; enable fuzzy auto complete
        ac-trigger-key "TAB"
        )

  ;; Use C-n/p instead of arrow keys to select ac options from the ac menu
  (setq ac-use-menu-map t)
  ;; Default settings
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)

  (ac-config-default)

  (defvar setup-auto-complete-loaded t)
  )

(provide 'setup_autocomplete)