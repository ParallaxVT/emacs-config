(defconst emacs-start-time (current-time))

;;------------------------------------------------------------------
;; Loading time
;;------------------------------------------------------------------

(when (and load-file-name)
  (let ((file-name (file-name-nondirectory load-file-name)))
    (let ((elapsed (float-time (time-subtract (current-time)
                                              emacs-start-time))))
      (message "Loading %s...done (%.3fs)" file-name elapsed))
    (add-hook 'after-init-hook
              `(lambda ()
                 (let ((elapsed (float-time (time-subtract (current-time)
                                                           emacs-start-time))))
                   (message "Loading %s...done (%.3fs) [after-init]"
                            ,file-name elapsed)))
              t)))
