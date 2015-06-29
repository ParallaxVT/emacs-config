(defconst  emacs-start-time (current-time))

(defvar emacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs directory.")
(defvar vendor-dir (expand-file-name "vendor" emacs-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar savefile-dir (expand-file-name "savefile" emacs-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar config-dir (expand-file-name "config" emacs-dir)
    "This file contains all the configuration files")

(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(defun add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (add-subfolders-to-load-path name)))))

(add-to-list 'load-path config-dir)

(require 'functions)

;;------------------------------------------------------------------
;; Package
;;------------------------------------------------------------------

(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
;; set package-user-dir to be relative to Prelude install path
(setq package-user-dir (expand-file-name "elpa" emacs-dir))
(package-initialize)

;;------------------------------------------------------------------
;; Load local config files 
;;------------------------------------------------------------------

;;(load settings-file)
;;(load packages-file)
(require 'settings)
(require 'packages)
(add-subfolders-to-load-path vendor-dir)
(get-time "vendor dir")

;; Set path to custom.el and then load it
(setq custom-file (expand-file-name "custom.el" config-dir))
(load custom-file 'noerror)
(get-time "custom.el")

;;------------------------------------------------------------------
;; Loading time
;;------------------------------------------------------------------

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
