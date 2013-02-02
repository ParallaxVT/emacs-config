;; Emacs configuration file

;; determine the load path dirs
;; as relative to the location of this file
;;Use eval-when-compile to avoid warning about require 'cl
(eval-when-compile (require 'cl))
;(require 'cl)

(defvar dotfiles-dir "~/.emacs/"
  "The root Emacs Lisp source folder")

;; external packages reside here
(defvar ext-dir (concat dotfiles-dir "vendor/")
  "The root folder for external packages")


(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the
Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; add the first level subfolders automatically
(add-subfolders-to-load-path dotfiles-dir)
(add-subfolders-to-load-path ext-dir)

(load "~/.emacs/conf/backup_and_autosave")
