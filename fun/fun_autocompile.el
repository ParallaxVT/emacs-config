;; ====================
;; fun_autocompile.el
;; ====================
(defun autocompile nil
  "Compile if init.el is newer than init.elc"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))
(provide 'fun_autocompile)
