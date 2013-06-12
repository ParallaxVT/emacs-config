;; ====================
;; fun_bytecompile.el
;; ====================
(defun bc nil
  "Merge files, byte-compile and reload init.elc"
  (interactive)

  (shell-command (concat "bash " dotfiles-dir "misc/makeinit.sh"))
  (load-file (concat dotfiles-dir "init.elc")))
(provide 'fun_bytecompile)
