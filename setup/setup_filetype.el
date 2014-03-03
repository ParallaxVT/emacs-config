;; ====================
;; setup_filetype.el
;; ====================

;; Asign a mayor mode depending on the file extension
(setq auto-mode-alist
      (append '(("\.xml$"  . sgml-mode))
              auto-mode-alist))

;; Use UTF-8-dos everywhere by default
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
(set-terminal-coding-system 'utf-8-dos)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-16-le))
(prefer-coding-system 'utf-8-dos)
(set-buffer-file-coding-system 'utf-8-dos)
(setq-default default-buffer-file-coding-system 'utf-8-dos)

(provide 'setup_filetype)
