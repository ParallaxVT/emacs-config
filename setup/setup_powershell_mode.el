;; ====================
;; setup_powershell_mode.el
;; ====================

(autoload 'powershell-mode "powershell-mode" "Mode PowerShell" t)
(push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist)

(provide 'setup_powershell_mode)