;; ====================
;; setup_autosave.el
;; ====================

(defvar user-temporary-autosave-directory (concat dotfiles-dir "autosave")
  "Directory to store all the autosave files")

(make-directory user-temporary-autosave-directory t) ; Create directory if doesn't exists already

(defvar
  auto-save-file-name-transforsm            ; place all auto-save files in a dedicated directory
  `((".*" ,user-temporary-autosave-directory t))
  )

(global-auto-revert-mode t)                ; revert buffers every 5 seconds

;; Remember my location in a file when saving files
(setq save-place-file (concat dotfiles-dir "autosave/saveplace"))
;; Savehist keeps track of some history
(setq savehist-additional-variables
      ;; Search entries
      '(search ring regexp-search-ring)
      ;; Save every minute
      savehist-autosave-interval 60
      ;; Keep the home clean
      savehist-file (concat dotfiles-dir "autosave/savehist"))
(savehist-mode t)
;; Save recent files
(setq recentf-save-file (concat dotfiles-dir "autosave/recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)

(provide 'setup_autosave)
