;; ====================
;; defun_bytecompile.el
;; ====================
(defun bc nil
  "Merge files within conf/ and defun/, and myte-compile ~/.emacs/init.el if is newer than init.elc"
  (interactive)
  (require 'bytecomp)

  (shell-command "cat /media/c/Users/rafaelgp/AppData/Roaming/.emacs/conf/*.el > C:/Users/rafaelgp/AppData/Roaming/.emacs/conf/all.tmp")
  (shell-command "cat /media/c/Users/rafaelgp/AppData/Roaming/.emacs/defun/*.el > C:/Users/rafaelgp/AppData/Roaming/.emacs/defun/all.tmp")
  (shell-command "cat /media/c/Users/rafaelgp/AppData/Roaming/.emacs/vendor/*.el > C:/Users/rafaelgp/AppData/Roaming/.emacs/vendor/all.tmp")
  (shell-command "cp /media/c/Users/rafaelgp/AppData/Roaming/.emacs/init_base.el /media/c/Users/rafaelgp/AppData/Roaming/.emacs/init.el")
  (shell-command "cat /media/c/Users/rafaelgp/AppData/Roaming/.emacs/conf/all.tmp >> C:/Users/rafaelgp/AppData/Roaming/.emacs/init.el")
  (shell-command "cat /media/c/Users/rafaelgp/AppData/Roaming/.emacs/defun/all.tmp >> C:/Users/rafaelgp/AppData/Roaming/.emacs/init.el")
  (shell-command "cat /media/c/Users/rafaelgp/AppData/Roaming/.emacs/vendor/vendor.el >> C:/Users/rafaelgp/AppData/Roaming/.emacs/init.el")
  (shell-command "dos2unix /media/c/Users/rafaelgp/AppData/Roaming/.emacs/init.el")
  (shell-command "rm /media/c/Users/rafaelgp/AppData/Roaming/.emacs/conf/all.tmp")
  (shell-command "rm /media/c/Users/rafaelgp/AppData/Roaming/.emacs/defun/all.tmp")
  (shell-command "rm /media/c/Users/rafaelgp/AppData/Roaming/.emacs/vendor/all.tmp")

(let ((dotemacs (expand-file-name "~/.emacs/init.el")))
        (byte-compile-file dotemacs)))
  
(provide 'defun_bytecompile)

