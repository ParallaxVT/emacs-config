;; ====================
;; mod_helm.el
;; ====================

(require 'helm-config)

(helm-mode 1)

(setq helm-buffer-max-length 40)
(setq helm-idle-delay .3)
(setq helm-input-idle-delay .3)

(provide 'mod_helm)
