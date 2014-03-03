;; ====================
;; setup_helm.el
;; ====================

(require 'helm-swoop)

(when (boundp 'setup-helm-loaded)
  (global-set-key (kbd "M-i") 'helm-multi-swoop-all)
  ;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch))

;; (require 'helm-config)
;; (require 'helm-command)
;; (require 'helm-elisp)
;; (require 'helm-misc)

;; (setq helm-idle-delay .4)
;; (setq helm-input-idle-delay .4)
;; (setq helm-candidate-number-limit 300)
;; (setq helm-samewindow nil)
;; (setq helm-quick-update t)

(helm-mode -1)
(setq setup-helm-loaded t)

(provide 'setup_helm)
