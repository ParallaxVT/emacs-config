;; ====================
;; Theme
;; ====================

(add-to-list 'custom-theme-load-path (concat vendor-dir "themes/"))
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

(fset 'yes-or-no-p 'y-or-n-p)                   ;; use 'y' and 'n' instoad 'Yes' and 'No'
(setq visible-bell t)                           ;; No bells and whistles
(setq-default default-justification 'none)      ;; Deactivate justification. Stops  split the lines
(setq-default mode-require-final-newline nil)   ;; Prevent adding a new line  at the end of a file
(defvar whitespace-style '(face trailing tabs)) ;; Something about  margins (white-space-line-column)
(delete-selection-mode t)                       ;; Delete selection with a keypress

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

