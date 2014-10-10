;; Emacs configuration file

(server-start)

(defconst emacs-start-time (current-time))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Enter in debug mode if there is an error
;; (setq debug-on-error t)

(defvar dotfiles-dir "~/.emacs.d/"
  "The root dir of my emacs files in Linux.")
;;    (defvar elpa-dir "~/.emacs.d/elpa/"
;;      "The directory for elpa packages in Linux.")

(defvar settings-dir (expand-file-name "setup/" dotfiles-dir)
  "The directory for emacs functionality.")
(defvar vendor-dir (expand-file-name "vendor/" dotfiles-dir)
  "The directory for external packages.")
(defvar elpa-dir (expand-file-name "elpa/" dotfiles-dir)
  "The directory for elpa packages.")
(defvar backup-dir (expand-file-name "backup/" dotfiles-dir))
(defvar autosave-dir (expand-file-name "autosave/" dotfiles-dir))
(defvar misc-dir (expand-file-name "misc/" dotfiles-dir))

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the Emacs load-path."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f "."))
                 (not (equal f ".git")))
        (add-to-list 'load-path name)))))

;; Create directories if don't exists already
(make-directory vendor-dir t)
(make-directory settings-dir t)
(make-directory autosave-dir t)

;; Add directories to load-path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path vendor-dir)
;;(add-to-list 'load-path elpa-dir)

;; Add all the subfoldres in the first level to load-path
(add-subfolders-to-load-path vendor-dir)

;; Bookmarks file location
(make-directory misc-dir t) ; Create directory if doesn't exists already
(defvar bookmark-file "~/.emacs.d/misc/bookmarks")
(defvar bookmark-default-file "~/.emacs.d/misc/bookmarks")
(defvar bmkp-default-bookmark-file "~/.emacs.d/misc/bookmarks")
(defvar bmkp-last-as-first-bookmark-file nil)

;;------------------------------------------------------------------
;; Package settings
;;------------------------------------------------------------------

;; (eval-and-compile
;; (require 'package)
;; (package-initialize t))

(require 'package)
(setq package-user-dir elpa-dir)

(setq package-archives '(
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;; ("ELPA" . "http://tromey.com/elpa/")
                         ;; ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install "use-package" if it isn't installed
(defvar my-packages '(use-package))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(eval-when-compile
  (require 'cl))

(require 'use-package)

;;------------------------------------------------------------------
;; Package settings
;;------------------------------------------------------------------

;; Theme
;; =================================================================

(add-to-list 'custom-theme-load-path (concat vendor-dir "themes/"))
(load-theme 'super-wombat t)
(defvar color-theme-is-global t)

(defvar custom-safe-themes t)
;; Font
;; =================================================================

(defconst win32-p (eq system-type 'windows-nt) "Windows OS.")
(defconst cygwin-p (eq system-type 'cygwin))
(defconst linux-p  (or (eq system-type 'gnu/linux)  (eq system-type 'linux)))

(defvar vsc-little-font "")

(when linux-p
  (setq vsc-little-font "Monospace-10.5"))

(when cygwin-p
  (setq vsc-little-font "Bitstream Vera Sans Mono-11"))

(when win32-p
  (setq vsc-little-font "Bitstream Vera Sans Mono-10.5"))

;; basic deffault appearance
(add-to-list 'default-frame-alist (cons 'font vsc-little-font))
;; new frame appearance; overides default-frame-alist
(add-to-list 'initial-frame-alist (cons 'font vsc-little-font))

;; Gui
;; =================================================================

(tool-bar-mode -1)                                 ;; Hide tool bar
(menu-bar-mode 1)                                  ;; Show menu bar
(blink-cursor-mode -1)                             ;; Cursor, stop blinking!
(setq inhibit-startup-screen (user-login-name))    ;; Start up screen? No thanks
(setq initial-scratch-message
      (concat ";; Initialization successful, welcome to "
              (substring (emacs-version) 0 16) ".")) ;; New scratch buffer text

;; Editor behavior
;; =================================================================

(fset 'yes-or-no-p 'y-or-n-p)                   ;; use 'y' and 'n' instoad 'Yes' and 'No'
(setq visible-bell t)                           ;; No bells and whistles
;;(setq-default default-justification 'none)    ;; Deactivate justification. Stops  split the lines
(setq-default mode-require-final-newline nil)   ;; Prevent adding a new line  at the end of a file
(defvar whitespace-style '(face trailing tabs)) ;; Something about  margins (white-space-line-column)
(delete-selection-mode t)                       ;; Delete selection with a keypress
(electric-pair-mode t)                          ;; Close autematically parenthesis and double quotes
(electric-indent-mode t)                        ;; Indent a line after pressing return
(show-paren-mode t)                             ;; Highlight matching parents
(defvar show-paren-style 'parenthesis)          ;; Set parenthesis as a parent
(global-hl-line-mode +1)                        ;; Highlight the current line
(setq next-line-add-newlines t)                 ;; Make sure text files end in a new line
(setq inhibit-default-init t)                   ;; Disable loading of defoult.el at startup

;; Buffer
;; =================================================================

(require 'uniquify)                        ;; Meaningful names for buffers with the same name
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)      ;; Rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")   ;; Don't muck with special buffers

;; Windows
;; =================================================================

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)                     ;; Smooth scrolling
(windmove-default-keybindings)                               ;; Switch between buffers using shift + arrows
(setq truncate-partial-width-windows nil)                    ;; Truncate lines that are too long
(setq frame-title-format '(buffer-file-name "%f" ("%b")))    ;;  Set the title bar to show file name if available, buffer name otherwise

;; Mode line
;; =================================================================

(line-number-mode t)     ;; Show line number
(column-number-mode t)   ;; Show column number
(size-indication-mode t) ;; Show buffer size in the mode line

;; Line numbers
;; =================================================================

(if (fboundp 'fringe-mode)
    (fringe-mode 5))  ;; Make the sides fringe thinner
(global-linum-mode t) ;; show line numbers please!

;; Indent
;; =================================================================

(setq tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)  ;; Always indent using spaces never tabs

;;------------------------------------------------------------------
;; Autosave and Backup Settings
;;------------------------------------------------------------------

;; Put autosaved and backup files in a specific directory
(setq
 backup-directory-alist (list (cons ".*" backup-dir))
 auto-save-list-file-prefix autosave-dir
 auto-save-file-name-transforms `((".*" ,autosave-dir t))
 backup-by-copying t     ;; Always use copying to create backup files
 delete-old-versions t   ;; Delete excess backup versions siletly
 kept-new-versions 6     ;; Number of oldest versions to keep when a new numbered backup is made
 kept-old-versions 2     ;; Number of newest versions to keep when a new numbered backup is made
 version-control t       ;; Make numeric backup versions
 )

;; Revert buffers every 5 seconds
(global-auto-revert-mode t)

;; Save cursor position
(require 'saveplace)
(setq save-place-file (concat autosave-dir "saveplace.el"))
(setq-default save-place t)

;; Savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; Search entries
      '(search ring regexp-search-ring)
      ;; Save every minute
      savehist-autosave-interval 60
      ;; Keep the home clean
      savehist-file (concat autosave-dir "savehist.el"))
(savehist-mode t)

;; Save recent files
(require 'recentf)
(setq recentf-save-file (concat autosave-dir "recentf.el")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)

;;------------------------------------------------------------------
;; Abbrev
;;------------------------------------------------------------------
(setq-default abbrev-mode t)
(setq abbrev-file-name (concat misc-dir "~/abbrev_defs"))
(setq save-abbrevs t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

;;------------------------------------------------------------------
;; FileType Settings
;;------------------------------------------------------------------

;; Asign a mayor mode depending on the file extension
(setq auto-mode-alist
      (append '(("\.xml$"  . sgml-mode))
              auto-mode-alist))

;; Use UTF-8-dos everywhere by default
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
(set-terminal-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-dos)
(set-buffer-file-coding-system 'utf-8-dos)
(setq-default default-buffer-file-coding-system 'utf-8-dos)
;; Clipboard
(set-clipboard-coding-system 'utf-8)

;;------------------------------------------------------------------
;; Eshell
;;------------------------------------------------------------------

(add-hook 'eshell-mode-hook
          (lambda ()
            (global-hl-line-mode -1)                        ;; Disable highlight the current line
            (setq eshell-ask-to-save-history 'always)
            (setq eshell-history-size 1000)
            (setq eshell-ask-to-save-history 'always)
            (setq eshell-cmpl-cycle-completions nil)
            (setq eshell-ls-dired-initial-args '("-h"))
            (setq eshell-ls-initial-args "-h")
            (setq eshell-ls-use-in-dired t)
            (setq eshell-output-filter-functions '(eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom))
            (setq eshell-scroll-show-maximum-output t)
            (setq eshell-scroll-to-bottom-on-output t)
            (setq eshell-glob-include-dot-dot nil)
            (setq eshell-directory-name (concat dotfiles-dir "eshell"))))
;;------------------------------------------------------------------
;; Hippie expand
;;------------------------------------------------------------------

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-romplete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Make rgrep find the executables it needs in windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setq find-program "C:\\\"Program Files (x86)\"\\Git\\bin\\find"
        xargs-program "C:\\\"Program Files (x86)\"\\Git\\bin\\xargs"
        grep-program "C:\\\"Program Files (x86)\"\\Git\\bin\\grep"))

(defadvice shell-quote-argument (after windows-nt-special-quote (argument) activate)
  "Add special quotes to ARGUMENT in case the system type is 'windows-nt."
  (when
      (and (eq system-type 'windows-nt) (w32-shell-dos-semantics))
    (if (string-match "[\\.~]" ad-return-value)
        (setq ad-return-value
              (replace-regexp-in-string
               "\\([\\.~]\\)"
               "\\\\\\1"
               ad-return-value)))))

;;------------------------------------------------------------------
;; Functions
;;------------------------------------------------------------------

(require 'setup_defun)

;;------------------------------------------------------------------
;; Package
;;------------------------------------------------------------------

(require 'use-packages)

;;------------------------------------------------------------------
;; Customize file
;;------------------------------------------------------------------

;; Set path to custom.el and then load it
(setq custom-file (expand-file-name "custom.el" settings-dir))
(load custom-file 'noerror)

;;------------------------------------------------------------------
;; Loading time
;;------------------------------------------------------------------

(when (and load-file-name)
  (let ((file-name (file-name-nondirectory load-file-name)))
    (let ((elapsed (float-time (time-subtract (current-time)
                                              emacs-start-time))))
      (message "Loading %s...done (%.3fs)" file-name elapsed))
    (add-hook 'after-init-hook
              `(lambda ()
                 (let ((elapsed (float-time (time-subtract (current-time)
                                                           emacs-start-time))))
                   (message "Loading %s...done (%.3fs) [after-init]"
                            ,file-name elapsed)))
              t)))

(put 'downcase-region 'disabled nil)
