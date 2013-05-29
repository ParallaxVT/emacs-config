;; =========================
;; defun_kill_ring_save.el
;; =========================
;; The equivalent of 'yy' or 'Y' in Vim
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
