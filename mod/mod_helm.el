;; ====================
;; mod_helm.el
;; ====================

(require 'helm-config)

(helm-mode 1)

(defvar helm-buffer-max-length 40)
(defvar helm-idle-delay 0.1)
(defvar helm-input-idle-delay 0.1)

(provide 'mod_helm)
