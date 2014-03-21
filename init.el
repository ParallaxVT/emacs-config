;; Emacs configuration file

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Enter in debug mode if there is an error
;; (setq debug-on-error t)

(defvar dotfiles-dir "~/.emacs.d/"
  "The root dir of my emacs files.")
(defvar backup-dir (expand-file-name "backup/" dotfiles-dir)
  "The directory for backup files.")
(defvar settings-dir (expand-file-name "setup/" dotfiles-dir)
  "The directory for emacs functionality.")
(defvar vendor-dir (expand-file-name "vendor/" dotfiles-dir)
  "The directory for external packages.")
(defvar elpa-dir (expand-file-name "elpa/" dotfiles-dir)
  "The directory for elpa packages.")

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the Emacs load-path."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f ".git")))
        (add-to-list 'load-path name)))))

;; Add directories to load-path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path elpa-dir)

;; Add all the subfoldres in the first level to load-path
(add-subfolders-to-load-path vendor-dir)

;; Settings
(require 'setup_appearance nil t)
(require 'setup_autosave nil t)
(require 'setup_backup nil t)
(require 'setup_filetype nil t)
(require 'setup_hooks nil t)

;; Functions
(require 'setup_defun nil t)

;; Modules -> setup_melpa ALWAYS FIRST
(require 'setup_melpa nil t)
(require 'setup_auto_highlight_symbol nil t)
;; (require 'setup_css nil t)
;; (require 'setup_cygwin nil t)
(require 'setup_dired_plus nil t)
;;(require 'setup_elget nil t)
(require 'setup_eshell nil t)
(require 'setup_evil_numbers nil t)
(require 'setup_evil nil t)
(require 'setup_fci nil t)
(require 'setup_flycheck nil t)
;; (require 'setup_fold nil t)
;; (require 'setup_git nil t)
;; (require 'setup_grep nil t)
(require 'setup_helm nil t)
(require 'setup_hippie_expand nil t)
(require 'setup_ido nil t)
(require 'setup_key_chord nil t)
;; (require 'setup_magit nil t)
;; (require 'setup_mmm_mode nil t)
(require 'setup_multiple_cursors nil t)
(require 'setup_org nil t)
(require 'setup_php_plus_mode nil t)
;; (require 'setup_powerline nil t)
(require 'setup_powershell_mode nil t)
(require 'setup_rainbow_mode nil t)
(require 'setup_rotate_text nil t)
(require 'setup_sgml_mode nil t)
;; (require 'setup_slime nil t)
(require 'setup_smex nil t)
(require 'setup_undo_tree nil t)
;; (require 'setup_volatile_highlights nil t)
(require 'setup_yasnippet nil t)

;; Vendor
(require 'auto-highlight-symbol nil t)
;; (require 'fold nil t)
(require 'linum-off nil t)
(require 'rotate-text nil t)

;; ==============================
;; Keybindings - always load last
;; ==============================

(require 'setup_keybindings nil t)

;; Set path to custom.el and then load it
(setq custom-file (expand-file-name "custom.el" settings-dir))
(load custom-file 'noerror)

;; TO DO
;; Create a prog-mode to use prog-mode-hook
;; There are some files waiting for this. Grep 'grog-mode-hook' to find out