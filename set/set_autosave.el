;; ====================
;; conf_autosave.el
;; ====================
(defvar user-temporary-autosave-directory (concat dotfiles-dir "autosave")
  "Directory to store all the autosave files")

(make-directory user-temporary-autosave-directory t) ; Create directory if doesn't exists already

(defvar
 auto-save-file-name-transforsm            ; place all auto-save files in a dedicated directory
`((".*" ,user-temporary-autosave-directory t))
 )

(global-auto-revert-mode t)                ; revert buffers every 5 seconds

(provide 'set_autosave)

