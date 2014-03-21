;; ====================
;; setup_keybindings.el
;; ====================

(global-set-key (kbd "C-c n")                  'cleanup-buffer)
(global-set-key (kbd "M-;")                    'comment-dwim-line)
(define-key emacs-lisp-mode-map (kbd "C-c v")  'eval-current-buffer)
(global-set-key [(meta shift up)]              'evil-numbers/inc-at-pt)
(global-set-key [(meta shift down)]            'evil-numbers/dec-at-pt)
(global-set-key (kbd "C-c g")                  'google-it)
(global-set-key (kbd "C-x C-b")                'helm-buffers-list)
(global-set-key (kbd "C-c r")                  'ibuffer)
(global-set-key (kbd "C-x C-f")                'ido-find-file)
(global-set-key (kbd "C-x f")                  'helm-recentf)
(global-set-key (kbd "M-/")                    'hippie-expand)
(global-set-key (kbd "C-M-\\")                 'indent-region-or-buffer)
(global-set-key (kbd "C-c C-k")                'kill-region)
(global-set-key [(control shift up)]           'move-line-up)
(global-set-key [(control shift down)]         'move-line-down)
(global-set-key (kbd "M-n")                    'negative-argument)
(global-set-key (kbd "C-c C-c")                'rotate-text)
(global-set-key (kbd "C-c '")                  'select-current-line)
(global-set-key [remap kill-whole-line]        'smart-kill-whole-line)
(global-set-key (kbd "C-c k")                  'smart-kill-other-buffers)
(global-set-key [remap move-beginning-of-line] 'smart-move-beginning-of-line)
(global-set-key [(shift return)]               'smart-open-line)
(global-set-key [(control shift return)]       'smart-open-line-above)
(global-set-key (kbd "C-c s")                  'smart-swap-windows)
(global-set-key (kbd "C-c SPC")                'yas/expand)
(global-set-key (kbd "C->")                    'mc/mark-next-like-this)
(global-set-key (kbd "C-<")                    'mc/mark-next-like-this)
(global-set-key (kbd "C-'")                    'mc/mark-all-like-this)
(global-set-key (kbd "C-}")                    'mc/edit-ends-of-lines)
(global-set-key (kbd "C-{")                    'mc/edit-beginnings-of-lines)

;; ================================
;; Key-chord
;; ================================
(when (require 'key-chord nil 'noerror)
  (key-chord-define-global "OO" 'other-window)
  (key-chord-define-global ",w" 'ace-jump-word-mode)
  (key-chord-define-global ",l" 'ace-jump-line-mode)
  (key-chord-define-global ",c" 'ace-jump-char-mode)
  (key-chord-define-global ",," 'smart-switch-to-previous-buffer)
  (key-chord-define-global "UU" 'undo-tree-visualize)
  (key-chord-define-global ";;" 'helm-bookmarks)
  (key-chord-define-global ".." 'helm-imenu)
  (key-chord-define-global ",d" 'duplicate-current-line-or-region)
  (key-chord-define-global "__" 'bookmark-bmenu-list)
  )

;; ================================
;; Evil
;; ================================

(when (boundp 'mod-evil-loaded)
  (global-set-key (kbd "M-SPC")                   'evil-normal-state)
  (evil-define-key 'normal global-map (kbd ", d") 'duplicate-current-line-or-region)
  ;; esc key quits everything
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  )

;; ================================
;; Obsolete
;; ================================

;; (global-key key (kbd "\C-ca")                 'org-agenda)
;; (global-key key (kbd "\C-cl")                 'org-store-link)
;; (global-set-key (kbd "C-c C-d")               'duplicate-current-line-or-region)
;; (global-set-key (kbd "C-c r")                 'smart-rename-file-and-buffer)
;; (global-set-key (kbd "C-x C-b")               'ibuffer)
;; (global-set-key (kbd "\C-cw")                 'fold-whole-buffer)  ;; close all folds
;; (global-set-key (kbd "\C-ca")                 'fold-show-all)      ;; open all folds
;; (global-set-key (kbd "\C-cs")                 'fold-show)          ;; open current fold
;; (global-set-key (kbd "\C-ch")                 'fold-hide)          ;; close current fold
;; (global-set-key (kbd "\C-cf")                 'fold-region)        ;; fold current region

(provide 'setup_keybindings)
