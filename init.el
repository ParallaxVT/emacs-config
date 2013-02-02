;; Emacs configuration file

;;Use eval-when-compile to avoid warning about require 'cl
(eval-when-compile (require 'cl))
;(require 'cl)

;; enter in debug mode if there is an error
(setq debug-on-error t)

;; root directory containing all the emacs configuration files
(defvar dotfiles-dir "~/.emacs/"
  "The root Emacs Lisp source folder")

;; external packages directory
(defvar ext-dir (concat dotfiles-dir "vendor/")
  "The root folder for external packages")

;; add to the load pah
(add-to-list 'load-path dotfiles-dir)

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f ".git")))
        (add-to-list 'load-path name)))))

;; add the first level subfolders automatically
(add-subfolders-to-load-path dotfiles-dir)
(add-subfolders-to-load-path ext-dir)

;; set custom.el path and load it
(setq custom-file (concat dotfiles-dir "conf/custom.el"))
(load custom-file 'noerror)

(load (concat dotfiles-dir "conf/conf_autosave"))
(load (concat dotfiles-dir "conf/conf_backup"))
