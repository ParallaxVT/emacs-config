(message "Loading Emacs!")

;;{{{ FIRST

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;;}}}
;;{{{ DIRECTORIES

;;Use eval-when-compile to avoid warning about require 'cl
(eval-when-compile (require 'cl))

;; Add vendor folder and its subdirectories to path
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Save customizations in this file and load it
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;}}}
;;{{{ PACKAGES

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;           '("marmalade" . "http://marmalade-repo.org/packages/") t)
(setq package-user-dir "~/.emacs.d/elpa")
(package-initialize)

(defvar packages-list
  '(expand-region magit magithub rainbow-mode undo-tree volatile-highlights)
  "A list of packages to ensure are installed at launch.")

(defun check-packages-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (check-packages-installed)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing package database")
  (package-refresh-contents)
  (message "%s" " Done!")
  ;; install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'packages-list)

(message "Packages options loaded...")
(setq max-specpdl-size 6000)

;;}}}
;;{{{ INTERFACE

;; Font
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

;; Cursor, stop blinking!
(blink-cursor-mode -1)
;; Start up screen is not cool
(setq inhibit-startup-screen (user-login-name))
;; Sweet scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
;; Show line number in the mode line
(line-number-mode t)
;; Show column number in the mode line
(column-number-mode t)
;; Show buffer size in the mode line
(size-indication-mode t)
;; Gimme line numbers please!
(global-linum-mode t)
;; Make the sides fringe thinner
(if (fboundp 'fringe-mode)
    (fringe-mode 1))
;; Why the scratch buffer needs 3 lines?
(setq initial-scratch-message ";; Scratch Buffer")
;; Switch between buffers using shift + arrows
(windmove-default-keybindings)
;; No bells and whistles
(setq visible-bell t)
;; Something to do with the margins (white-space-line-column)
(setq whitespace-style '(face trailing tabs))
;; Deactivate justification. It drives me mad when the code splited
(setq-default default-justification 'none)

;; use 'y' and 'n' instoad 'Yes' and 'No'
(fset 'yes-or-no-p 'y-or-n-p)

(require 'color-theme)
(require 'color-theme-wombat)
(color-theme-wombat)

(message "Interface options loaded...")

;;}}}
;;{{{ EDITOR

;; don't use tabs to indent
(setq-default indent-tabs-mode nil)
;; but maintain correct appearance
(setq-default tab-width 8)
;; delete the selection with a keypress
(delete-selection-mode t)
;; Put all backup and autosave files in ~/tmp dirirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; revert buffers every 5 seconds just in case the file has been changed externally
(global-auto-revert-mode t)
;; Use hippie-expand
(setq hippie-expand-try-functions-list '(try-complete-file-name
                                         try-complete-file-name-partially
                                         try-complete-lisp-symbol
                                         try-complete-lisp-symbol-partially
                                         try-expand-all-abbrevs
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-line
                                         try-expand-list))
;; close autematically parenthesis and double quotes
(electric-pair-mode t)
;; indent a line after pressing return
(electric-indent-mode t)
;; highlight matching parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
;; remember my location in a file when saving files
(setq save-place-file (concat user-emacs-directory "auto-save-list/saveplace"))
;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat user-emacs-directory "auto-save-list/savehist"))
(savehist-mode t)
;; save recent files
(setq recentf-save-file (concat user-emacs-directory "auto-save-list/recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)
;; highlight the current line
(global-hl-line-mode +1)


(message "Editor options loaded...")

;;}}}
;;{{{ FUNCTIONS

(defun visit-init ()
  "as in Load Custom"
  (interactive)
  (find-file "~/.emacs.d/init.el")
  )

;; Seach in Google
(defun google-is-your-friend ()
  "Search the text in a region in google."
  (interactive)
  (browse-url
   (concat
    "http://www.google.co.uk/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

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

;; The equivalent of 'yy' or 'Y' in Vim
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; The equivalent of 'dd' or 'D' in Vim
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Indent a region or the entire buffer
(defun indent-region-or-buffer ()
  "Indents a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

;; The equivalent of 'yy pp' in Vim
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; indent current buffer
(defun indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
;; replace tabs with spaces
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (whitespace-cleanup))

(defun autocompile nil
  "compile itself if ~/.emacs.d/init.el is newer than init.elc"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs.d/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs))))

(message "Functions loaded...")

;;}}}
;;{{{ HOOKS

(add-hook 'before-save-hook 'whitespace-cleanup nil t)
(add-hook 'after-save-hook 'autocompile)

(message "Hooks loaded...")

;;}}}
;;{{{ PACKAGES

;;{{{ EVIL

(require 'evil)
(evil-mode 1)
;;}}}
;;{{{ EL-GET

;; Downlead el-get if it's not installed
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") (unless (require 'el-get nil t) (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" (lambda (s) (goto-char (point-max)) (eval-print-last-sexp))))

;;}}}
;;{{{ HELM

(require 'helm-config)
;; Use helm completion it M-x, C-x C-f, etc...
;; (helm-mode 1)

;;}}}
;;{{{ MOM-CSS-COLOR

(add-hook 'css-mode-hook 'css-color-mode)
(autoload 'css-mode "css-mode" "" t)
(autoload  'css-color-mode "mon-css-color" "" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;}}}
;;{{{ EVIL NUMBERS

(require 'evil-numbers)

;;}}}
;;{{{ FOLDING

(require 'fold)
(add-hook 'sgml-mode-hook 'fold-mode)
(add-hook 'nxml-mode-hook 'fold-mode)
(add-hook 'css-mode-hook 'fold-mode)
(add-hook 'emacs-lisp-mode-hook 'fold-mode)

;;}}}
;;{{{ UNBOUND KEYS

;; shows keys available to bind
;; M-x load-library unbound
;; M-x describe-unbound-keys

;;}}}
;;{{{ PHP MODE

(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;}}}
;;{{{ KEYBINDINGS

(global-set-key (kbd "C-c n")                 'cleanup-buffer)
(global-set-key (kbd "C-c g")                 'google-is-your-friend)
(global-set-key (kbd "C-c h")                 'helm-mini)
(global-set-key (kbd "C-x C-f")               'helm-find-files)
(global-set-key (kbd "C-x f")                 'helm-recentf)
(global-set-key (kbd "M-/")                   'hippie-expand)
(global-set-key (kbd "C-x C-b")               'ibuffer)
(global-set-key (kbd "C-M-\\")                'indent-region-or-buffer)
(global-set-key [(control shift up)]          'move-line-up)
(global-set-key [(control shift down)]        'move-line-down)
(global-set-key (kbd "C-c C-c")               'rotate-text)
(global-set-key (kbd "C-c m")                 'visit-init)
(global-set-key (kbd "\C-cw")                 'fold-whole-buffer)  ;; close all folds
(global-set-key (kbd "\C-ca")                 'fold-show-all)      ;; open all folds
(global-set-key (kbd "\C-cs")                 'fold-show)          ;; open current fold
(global-set-key (kbd "\C-ch")                 'fold-hide)          ;; close current fold
(global-set-key (kbd "\C-cf")                 'fold-region)        ;; fold current region
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-current-buffer)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-;") 'evil-numbers/dec-at-pt)

(message "Keybindings loaded...")
;;}}}
;;{{{ YASNIPPET

;;(add-to-list 'load-path  "~/.emacs.d/elpa/yasnippet-0.6.1/")
(require 'yasnippet)

(add-hook 'html-mode-hook 'yas/minor-mode-on)
(add-hook 'css-mode-hook 'yas/minor-mode-on)
(add-hook 'php-mode-hook 'yas/minor-mode-on)
(add-hook 'xml-mode-hook 'yas/minor-mode-on)

(setq yas/root-directory "~/.emacs.d/snippets/")

(yas/load-directory yas/root-directory)

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;;}}}
;;{{{ ESHELL


(setq eshell-ask-to-save-history (quote always))
(setq eshell-history-size 1000)
(setq eshell-ask-to-save-history 'always)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-ls-dired-initial-args (quote ("-h")))
(setq eshell-ls-initial-args "-h")
(setq eshell-ls-use-in-dired t)
;; scroll to the bottom
(setq eshell-output-filter-functions (quote (eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom)))
(setq eshell-scroll-show-maximum-output t)
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-glob-include-dot-dot nil)

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;}}}
;;{{{ ROTOTE TEXT

(autoload 'rotate-text "rotate-text" nil t)
(autoload 'rotate-text-backward "rotate-text" nil t)

(defcustom rotate-text-words '(("width" "height")
                               ("plugin" "hotspot")
                               ("yes" "no")
                               ("true" "false")
                               ("left" "right" "top" "bottom"))
  "*List of words to rotate.
Each element is a list of words that should be cycled through.  Individual
segments in symbol names are recognized as words, i.e. windowWidth can be
replaced with windowHeight.
All entries must be in lower case. The case is determined by the rotated
text."
  :group 'rotate-text
  :type '(repeat (repeat :tag "Rotation group" (string :tag "Word"))))

;; }}}

(message "Pacakges options loaded...")


;;{{{ LAST
(menu-bar-mode +1)
(scroll-bar-mode +1)
;;}}}

(message "Emacs Loaded!")

