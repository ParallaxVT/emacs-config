(defun autocompile nil
  "Compile if ~/.emacs/init.el is newer than init.elc"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))

(add-hook 'after-save-hook 'autocompile)

(provide 'defun_autocompile)
