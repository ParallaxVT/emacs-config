(message "Loading Emacs!")

(require 'cl)

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs")
(defvar vendor-dir (concat root-dir "vendor/")
  "Directory with the packages not available in ELPA, Marmalade or el-get")
(add-to-list 'load-path vendor-dir)
;; Add vendor folder and its subdirectories to path
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))

;; =========== Packages ===========

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; I use a different directory because elpa is stuffed of Emacs-prelude packages
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

;; =========== Interface ===========

;; Font
(defconst win32p  (eq system-type 'windows-nt))
(defconst cygwinp (eq system-type 'cygwin))
(defconst linuxp  (or (eq system-type 'gnu/linux)  (eq system-type 'linux)))

(defvar vsc-little-font "")

(when linuxp
  (setq vsc-little-font "Bitstream Vera Sans Mono-11"))

(when cygwinp
  (setq vsc-little-font "Bitstream Vera Sans Mono-11"))

(when win32p
  (setq vsc-little-font "Bitstream Vera Sans Mono-10"))

;; basic deffault appearance
(add-to-list 'default-frame-alist (cons 'font vsc-little-font))
;; new frame appearance; overides default-frame-alist
(add-to-list 'initial-frame-alist (cons 'font vsc-little-font))

;; Get rid of the toolbar
(tool-bar-mode -1)
;; Cursor, stop blinking!
(blink-cursor-mode -1)
;; Start up screen is not cool
(setq inhibit-startup-screen t)
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
(setq initial-scratch-message "Scratch Buffer")
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

(load-theme 'wombat t)

(message "GUI options loaded...")

;; ========== Functions ==========

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
  (previous-line 2))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

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
(defun prelude-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (whitespace-cleanup))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; =========== HOOKS ==========

(add-hook 'before-save-hook 'whitespace-cleanup nil t)

;; =========== EDITOR ==========

;; Put all backup and autosave files in ~/tmp dirirectory
(setq backup-directory-alist
      '((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      '((".*" ,temporary-file-directory t)))

;; ========== KEYBINDINGS ==========

(define-key global-map (kbd "C-c n")            'cleanup-buffer)
(define-key global-map (kbd "C-c g")		'google-is-your-friend)
(define-key global-map (kbd "C-M-\\")		'indent-region-or-buffer)
(define-key global-map [(control shift up)]	'move-line-up)
(define-key global-map [(control shift down)]	'move-line-down)
(define-key global-map (kbd "C-c f")            'recentf-ido-find-file)
(define-key global-map (kbd "C-c m")		'visit-init)

;; ==============================
;;            PACKAGES
;; ==============================

;; ========== EVIL ==========

(require 'evil)
(evil-mode 1)

(message "Emacs Loaded!")

;; ========== IDO ==========

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)

 ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
