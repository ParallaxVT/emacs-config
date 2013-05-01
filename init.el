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

;; ====================
;; Theme
;; ====================

(add-to-list 'custom-theme-load-path "~/.emacs/vendor/themes/")
(load-theme 'super-wombat t)
(defvar color-theme-is-global t)

;; ====================
;; Font
;; ====================

(defconst win32-p (eq system-type 'windows-nt) "Windows OS.")
(defconst cygwin-p (eq system-type 'cygwin))
(defconst linux-p  (or (eq system-type 'gnu/linux)  (eq system-type 'linux)))

(defvar vsc-little-font "")

(when linux-p
  (setq vsc-little-font "Bitstream Vera Sans Mono-11"))

(when cygwin-p
  (setq vsc-little-font "Bitstream Vera Sans Mono-11"))

(when win32-p
  (setq vsc-little-font "Bitstream Vera Sans Mono-10.5"))

;; basic deffault appearance
(add-to-list 'default-frame-alist (cons 'font vsc-little-font))
;; new frame appearance; overides default-frame-alist
(add-to-list 'initial-frame-alist (cons 'font vsc-little-font))

;; ====================
;; Emacs gui
;; ====================

(tool-bar-mode -1)                                 ;; No tool bar
(blink-cursor-mode -1)                             ;; Cursor, stop blinking!
(setq inhibit-startup-screen (user-login-name))    ;; Start up screen? No thanks
(setq initial-scratch-message ";; Scratch Buffer") ;; New scratch buffer text

;; ====================
;; Emacs behavior
;; ====================

(fset 'yes-or-no-p 'y-or-n-p)                 ;; use 'y' and 'n' instoad 'Yes' and 'No'
(setq visible-bell t)                         ;; No bells and whistles
(setq-default default-justification 'none)    ;; Deactivate justification. Stops  split the lines
(setq-default mode-require-final-newline nil) ;; Prevent adding a new line  at the end of a file
(defvar whitespace-style '(face trailing tabs)) ;; Something about  margins (white-space-line-column)

;; ====================
;; Windows
;; ====================

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)  ;; Smooth scrolling
(windmove-default-keybindings)            ;; Switch between buffers using shift + arrows
(setq truncate-partial-width-windows nil) ;; Truncate lines that are too long

;; ====================
;; Mode line
;; ====================

(line-number-mode t)     ;; Show line number 
(column-number-mode t)   ;; Show column number
(size-indication-mode t) ;; Show buffer size in the mode line

;; ====================
;; Line numbers
;; ====================

(if (fboundp 'fringe-mode)
    (fringe-mode 1))  ;; Make the sides fringe thinner
(global-linum-mode t) ;; show line numbers please!


(provide 'conf_appearance)

;; ====================
;; conf_autosave.el
;; ====================
(defvar user-temporary-autosave-directory (concat dotfiles-dir "autosave")
  "Directory to store all the autosave files")

(make-directory user-temporary-autosave-directory t) ; Create directory if doesn't exists already

(defvar
 auto-save-file-name-transforsm            ; place all auto-save files in a dedicated directory
`((".*" ,user-temporary-autosave-directory t))
 )

(global-auto-revert-mode t)                ; revert buffers every 5 seconds

(provide 'conf_autosave)

;; ====================
;; conf_backup.el
;; ====================
(defvar user-temporary-backup-directory (concat dotfiles-dir "backup")
  "Directory to store all the backup files")

(make-directory user-temporary-backup-directory t) ; Create directory if doesn't exists already

(setq
 backup-by-copying t                       ; always use copying to create backup files
 backup-directory-alist                    ; alist of filename patterns and backup directory names
 `(("." . ,user-temporary-backup-directory)) ; (REGEXP . DIRECTORY)
 delete-old-versions t                     ; delete excess backup versions siletly
 kept-new-versions 6                       ; number of oldest versions to keep when a new numbered backup is made
 kept-old-versions 2                       ; number of newest versions to keep when a new numbered backup is made
 version-control t                         ; make numeric backup versions
 )

(provide 'conf_backup)

;; ====================
;; conf_css.el
;; ====================
;; ====================
;; conf_cygwin.el
;; ====================
;; ====================
;; conf_dired.el
;; ====================
;; ====================
;; conf_elget.el
;; ====================
;; ====================
;; conf_elpa.el
;; ====================
(defvar package-user-dir (concat dotfiles-dir "elpa"))
;; ====================
;; conf_eshell.el
;; ====================
;; ===============================
;; conf_fill_column_indicator.el
;; ===============================
;; Custom settings
(defvar fci-rule-width 2)
(defvar fci-rule-color "darkgreen")
(defvar fci-rule-column 80)
;; Turn on fci-mode automatically for certain mades
(defun my-add-to-multiple-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(my-add-to-multiple-hooks
 'fci-mode
 '(text-mode-hook
   emacs-lisp-mode-hook
   nxml-mode-hook
   css-mode-hook
   sh-mode
   fundamental-mode-hook))
;; ====================
;; conf_flyspell.el
;; ====================;; ====================
;; conf_git.el
;; ====================
;; ====================
;; conf_grep.el
;; ====================
;; ====================
;; conf_keybindings.el
;; ====================
(global-set-key (kbd "C-c C-k")               'kill-region) 

;; ====================
;; conf_magit.el
;; ====================
;; ====================
;; conf_mmm.el
;; ====================;; ====================
;; conf_org.el
;; ====================;; ====================
;; conf_package.el
;; ====================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;           '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; ====================
;; conf_php.el
;; ====================;; ====================
;; conf_powerline.el
;; ====================;; ====================
;; conf_slime.el
;; ====================
;; ====================
;; conf_smex.el
;; ====================
;; Show frequently used commands that have no key bindings 
;; smex-show-unbound-commands

;; Put smex file in misc folder
(defvar smex-save-file (concat dotfiles-dir "misc/.smex-items")) 

;; ====================
;; conf_yasnippet.el
;; ====================
;; ====================
;; custom.el
;; ====================;; ====================
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

;; ====================
;; defun_googleit.el
;; ====================
(defun google-it ()
  "Search the text in a region in google."
  (interactive)
  (browse-url
   (concat
    "http://www.google.co.uk/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))
;; ====================
;; defun_kill_region.el
;; ====================
;; The equivalent of 'dd' or 'D' in Vim
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;; =========================
;; defun_kill_ring_save.el
;; =========================
;; The equivalent of 'yy' or 'Y' in Vim
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;; ====================
;; defun_move_line.el
;; ====================
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
;; =======================
;; defun_visit_bashrc.el
;; =======================
(defun visit-bashrc ()
  "as in Load Custom"
  (interactive)
  (find-file "~/.bashrc")
  )
;; =====================
;; defun_visit_init.el
;; =====================
(defun visit-init ()
  "as in Load Custom"
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )
; for loading libraries in from the vendor directory
(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs/vendor/" file))
         (suffix (concat normal ".el")))
    (cond
     ((file-directory-p normal)
      (add-to-list 'load-path normal)
      (require library))
     ((file-directory-p suffix)
      (add-to-list 'load-path suffix)
      (require library))
     ((file-exists-p suffix)
      (require library)))))

(vendor 'fill-column-indicator)
(vendor 'linum-off)
