;; Emacs configuration file

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;Use eval-when-compile to avoid warnings about require 'cl
(eval-when-compile (require 'cl))

;; Enter in debug mode if there is an error
(setq debug-on-error t)

(defvar dotfiles-dir "~/.emacs/"
  "The root dir of my emacs files.")
(defvar backup-dir (expand-file-name "backup/" dotfiles-dir)
  "The directory for backup files.")
(defvar functions-dir (expand-file-name "fun/" dotfiles-dir)
  "The directory for fun.")
(defvar modules-dir (expand-file-name "mod/" dotfiles-dir)
  "The directory for modules.")
(defvar settings-dir (expand-file-name "set/" dotfiles-dir)
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
(add-to-list 'load-path functions-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path elpa-dir)

;; Add all the subfoldres in the first level to load-path
(add-subfolders-to-load-path vendor-dir)

;; Settings
(require 'set_appearance)
(require 'set_autosave)
(require 'set_backup)
(require 'set_keybindings)

;; Functions
(require 'fun_autocompile)
(require 'fun_bytecompile)
(require 'fun_google_it)
(require 'fun_kill_region)
(require 'fun_kill_ring_save)
(require 'fun_move_line)
(require 'fun_visit_bashrc)
(require 'fun_visit_init)

;; Modules 
(require 'mod_css)
(require 'mod_cygwin)
(require 'mod_dired)
(require 'mod_elget)
(require 'mod_elpa)
(require 'mod_eshell)
(require 'mod_fci)
(require 'mod_flyspell)
(require 'mod_git)
(require 'mod_grep)
(require 'mod_hippie_expand)
(require 'mod_magit)
(require 'mod_mmm)
(require 'mod_org)
(require 'mod_php)
(require 'mod_powerline)
(require 'mod_sgml_mode)
(require 'mod_slime)
(require 'mod_smex)
(require 'mod_yasnippet)

;; Vendor
(require 'fill-column-indicator)
(require 'linum-off)

;; Set path to custom.el and then load it
(setq custom-file (expand-file-name "custom.el" settings-dir))
(load custom-file 'noerror)
