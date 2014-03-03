;; ====================
;; setup_dired_plus.el
;; ====================

;; Enable ls-lisp
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
;; Hide 'Link Count' 'User' 'Group'
(setq ls-lisp-verbosity nil)
(require 'dired+)
;; Use always the same buffer
;; (toggle-dired-find-file-reuse-dir 1)
(toggle-diredp-find-file-reuse-dir 1)

;; When moving to parent directory by default creates a new buffer.The following rebinds Shift-SpaceBar to use the same buffer.
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "S-<SPC>")
              (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
            ))

(provide 'setup_dired_plus)



