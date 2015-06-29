;;; packages.el --- use-packages configuration file

;; Required for use-packages
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------

;;------------------------------------------------------------------
;; Hooks
;;------------------------------------------------------------------

;;------------------------------------------------------------------
;; Appearance
;;------------------------------------------------------------------

(use-package bind-key
  :ensure t
  :config
  (progn
    (bind-key "C-x C-f"                             'helm-find-files)
    (bind-key "C-x C-b"                             'helm-buffers-list)
    (bind-key "C-x f"                               'helm-recentf)
    (bind-key "C-x B"                               'helm-bookmark)
    ))

(use-package helm
  :ensure t
  :defer t
  :commands (helm-buffers-list
             helm-colors
             helm-find-files
             helm-for-files
             helm-google-suggest
             helm-mini
             helm-help
             helm-show-kill-ring
             helm-org-keywords
             helm-org-in-buffer-headings
             helm-projectile
             helm-M-x
             helm-occur)
  :config
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 30
          helm-ff-skip-boring-files t
          helm-idle-delay 0.0
          helm-quick-update t
          helm-buffers-fuzzy-matching t)
    ;; (helm-match-plugin-mode)
    (setq helm-boring-file-regexp-list
          '("\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.png$" "\\.swf$" "\\.sa$" "\\.fla$" "\\.elc"))
    (bind-key "C-w" 'helm-find-files-up-one-level helm-map)
    (bind-key "C-v" 'helm-execute-persistent-action helm-map)
    (use-package helm-ag
      :ensure t
      :defer t
      :commands (helm-ag)
      :if window-system
      :config (add-to-list 'exec-path "~/Ag"))
    (use-package helm-descbinds
      :ensure t
      :defer t
      :commands (helm-descbinds)
      :init
      (progn
        (helm-descbinds-mode)
        (setq helm-descbinds-window-style 'split-window))
      :bind ("C-x b" . helm-descbinds))
    ))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)    
  ))

(get-time "Packages")

(provide 'packages)
;;; packages.el ends here
