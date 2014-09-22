;;;###autoload
(defun powerline-evil-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (face3 (if active 'powerline-active3 'powerline-inactive2))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (lhs
              (append
               (let ((evil-face (powerline-evil-face active)))
                 (list 
                  (powerline-raw (powerline-evil-tag) evil-face)
                  (funcall separator-left evil-face face3)))
               (list (powerline-raw " " face3)
                     (powerline-raw mode-line-mule-info face3 'l)
                     (powerline-client face3)
                     (powerline-remote face3)
                     (powerline-frame-id face3)
                     (powerline-buffer-id face3 'l)
                     (powerline-raw " " face3)
                     (funcall separator-left face3 face2)
                     (powerline-raw mode-line-modified face2 'l)
                     (powerline-raw " " face2)
                     (funcall separator-left face2 face1)
                     (when (boundp 'erc-modified-channels-object)
                       (powerline-raw erc-modified-channels-object face1 'l))
                     (powerline-raw " " face1)
                     (powerline-raw
                      (if (and (boundp 'mode-line-debug-mode) mode-line-debug-mode)
                          (mode-line-debug-control)
                        " ")
                      face1)
                     (powerline-recursive-left face1)
                     (powerline-major-mode face1)
                     (powerline-process face1)
                     (powerline-minor-modes face1 'l)
                     (powerline-narrow face1 'l)
                     (powerline-recursive-right face1)
                     (powerline-raw " " face1)
                     (funcall separator-left face1 face2)
                     (powerline-vc face2 'r))))
             (rhs
              (append
               (when (and (boundp 'which-function-mode) which-function-mode)
                 (list
                  (powerline-raw "[" face2)
                  (powerline-which-func)
                  (powerline-raw "]" face2)))
               (list
                (when (and (boundp 'wc-mode) wc-mode)
                  (powerline-wc-mode face2 'r))
                (funcall separator-right face2 face1)
                (powerline-raw "  " face1)
                (powerline-raw global-mode-string face1 'r)
                (powerline-raw " " face1)
                (funcall separator-right face1 face2)
                (powerline-position face2 'r)
                (powerline-hud face2 face1)))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(provide 'powerline-evil-theme)

