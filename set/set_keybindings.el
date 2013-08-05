;; ====================
;; set_keybindings.el
;; ====================

(global-set-key (kbd "C-c n")                 'cleanup-buffer)
(global-set-key (kbd "M-;")                   'comment-dwim-line)
(global-set-key (kbd "C-c d")                 'duplicate-current-line-or-region)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-current-buffer)
(global-set-key (kbd "C-c g")                 'google-it)
(global-set-key (kbd "C-c r")                 'helm-mini)
(global-set-key (kbd "C-x C-f")               'helm-find-files)
(global-set-key (kbd "C-x f")                 'helm-recentf)
(global-set-key (kbd "M-/")                   'hippie-expand)
(global-set-key (kbd "C-x C-b")               'ibuffer)
(global-set-key (kbd "C-M-\\")                'indent-region-or-buffer)
(global-set-key (kbd "C-c C-k")               'kill-region)
(global-set-key [(control shift up)]          'move-line-up)
(global-set-key [(control shift down)]        'move-line-down)
(global-set-key (kbd "M-n")                   'negative-argument)
(global-set-key (kbd "C-c C-c")               'rotate-text)
(global-set-key (kbd "C-c '")                 'select-current-line)
(global-set-key (kbd "C-c SPC")               'yas/expand)

;; NOTE: key-chord keybindings defined in mod/mod_key_chord.el

;; (global-set-key (kbd "C-x C-b")               'ibuffer)
;; (global-set-key (kbd "C-a")                   'evil-numbers/inc-at-pt)
;; (global-set-key (kbd "C-;")                   'evil-numbers/dec-at-pt)
;; (global-set-key (kbd "\C-cw")                 'fold-whole-buffer)  ;; close all folds
;; (global-set-key (kbd "\C-ca")                 'fold-show-all)      ;; open all folds
;; (global-set-key (kbd "\C-cs")                 'fold-show)          ;; open current fold
;; (global-set-key (kbd "\C-ch")                 'fold-hide)          ;; close current fold
;; (global-set-key (kbd "\C-cf")                 'fold-region)        ;; fold current region

(provide 'set_keybindings)
