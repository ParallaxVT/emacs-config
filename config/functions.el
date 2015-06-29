;;; functions.el --- My custom functions

;;------------------------------------------------------------------
;; Loading times
;;------------------------------------------------------------------

(defun get-time (package-loaded)
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "--> %s (%.3fs)" package-loaded elapsed)))

(get-time "functions.el")

(provide 'functions)
;;; functions.el ends here
