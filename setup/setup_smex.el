;; ====================
;; setup_smex.el
;; ====================
;; Show frequently used commands that have no key bindings
;; smex-show-unbound-commands

(require 'smex)
;; Put smex file in misc folder
(defvar smex-save-file (concat dotfiles-dir "misc/.smex-items"))

(smex-initialize)
(setq setup-smex-loaded t)

(provide 'setup_smex)
