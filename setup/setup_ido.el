;; ====================
;; setup_ido.el
;; ====================

(require 'ido)

(setq ido-enable-flex-matching t ;; enable fuzzy search
      ido-everywhere t
      ido-create-new-buffer 'always ;; create a new buffer if no buffer matches substring
      ido-file-extensions-order '(".sv" ".v" ".svh" ".tv" ".m" ".c" ".cpp" ".emacs")
      ;;  customize the order in which files are sorted when Ido displays them in
      ;; the minibuffer. There are certain file extensions I use more than others,
      ;; so I tell Ido to emphasize those
      ido-use-filename-at-point 'guess ;; find file at point using ido
      ido-auto-merge-work-directories-length 0 ;; look into other directories if
      ;; the entered filename doesn't exist in current directory
      ;; ido-auto-merge-work-directories-length -1 ;; do NOT look into other directories if
      ;;                  ;; the entered filename doesn't exist in current directory
      )
(ido-mode 1)

;; Use flx-ido for better flex matching between words
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights

;; flx-ido looks better with ido-vertical-mode
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(provide 'setup_ido)
