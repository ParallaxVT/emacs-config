;; ====================
;; mod_elget.el
;; ====================

(defvar el-get-dir (expand-file-name "el-get/" dotfiles-dir)
  "Custom directory for el-get")

(add-to-list 'load-path (concat dotfiles-dir "el-get/el-get"))

;; Downlead el-get if it's not installed
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (concat dotfiles-dir "el-get-user/recipes"))

(el-get 'sync)

(provide 'mod_elget)