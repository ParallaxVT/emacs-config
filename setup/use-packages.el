;;------------------------------------------------------------------
;; Use-Packages
;;------------------------------------------------------------------

;; Function to create hooks with ease
(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;; (require 'bind-key)
(use-package bind-key
  :disabled nil
  :ensure t)

;; Hooks
(add-hook 'before-save-hook 'whitespace-cleanup nil t)
(add-hook 'after-save-hook 'autocompile)
(add-hook 'html-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq sgml-basic-offset 4)
             (setq indent-tabs-mode nil)))
(add-hook 'sgml-hook
          '(lambda ()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq sgml-basic-offset 4)
             (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
             (setq indent-tabs-mode nil)))
(add-hook 'xml-mode-hook
          '(lambda()
             (setq tab-width 4)
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)))

;; Keybindings
(bind-key "C-c n"                             'cleanup-buffer)
(bind-key "M-;"                               'comment-dwim-line)
(bind-key "C-c g"                             'google-it)
(bind-key "C-c r"                             'ibuffer)
(bind-key "M-/"                               'hippie-expand)
(bind-key "C-M-\\"                            'indent-region-or-buffer)
(bind-key "C-c C-k"                           'kill-region)
(bind-key "M-n"                               'negative-argument)
(bind-key "C-c k"                             'smart-kill-other-buffers)
(bind-key "C-c s"                             'smart-swap-windows)
(bind-key [(control shift up)]                'md/move-lines-up)
(bind-key [(control shift down)]              'md/move-lines-down)
(bind-key [remap kill-whole-line]             'smart-kill-whole-line)
(bind-key [remap move-beginning-of-line]      'smart-move-beginning-of-line)
(bind-key [(shift return)]                    'smart-open-line)
(bind-key [(control shift return)]            'smart-open-line-above)
(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)
(bind-key "C-M-g"                             'gforces-config)
(bind-key "C-x C-c"                           'suspend-frame)
(bind-key "C-x C-S-C"                         'save-buffers-kill-terminal)

(use-package key-chord
  :disabled t
  :ensure t
  :init (key-chord-mode 1)
  :config
  (progn
    (key-chord-define-global "OO" 'other-window)
    (key-chord-define-global ",," 'smart-switch-to-previous-buffer)
    (key-chord-define-global "UU" 'undo-tree-visualize)
    (key-chord-define-global ",d" 'duplicate-current-line-or-region)
    (key-chord-define-global "__" 'bookmark-bmenu-list)))

(use-package ace-jump-mode
  :disabled nil
  :ensure t
  :defer t
  :commands (ace-jump-mode key-chord-mode))

(use-package auto-complete
  :disabled nil
  :ensure t
  :commands (auto-complete)
  :init
  (progn
    (hook-into-modes #'auto-complete-mode '(lisp-mode-hook
                                            prog-mode-hook
                                            web-mode-hook)))
  :config
  (progn
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/misc/ac-dict")
    (setq ac-auto-start 'nil
          ac-comphist-file  "~/.emacs.d/misc/ac-comphist.dat"
          ac-ignore-case t ;; ignore case
          ac-stop-words '("/" "//" "/*" "//*" "///" "////")
          ac-trigger-key "M-="
          ac-use-fuzzy t ;; enable fuzzy auto complete
          ac-use-menu-map t)

    ;; Use C-n/p instead of arrow keys in the ac menu
    (bind-key "\C-n" 'ac-next ac-menu-map)
    (bind-key "\C-p" 'ac-previous ac-menu-map)))

(use-package auto-highlight-symbol
  :disabled t
  :init
  (progn
    (hook-into-modes #'auto-highlight-symbol-mode '(lisp-mode-hook
                                                    prog-mode-hook
                                                    web-mode-hook))))

(use-package bookmark+
  :disabled nil
  :commands (helm-bookmarks bookmark-bmenu-list)
  :ensure t)

(use-package dired+
  :disabled nil
  :ensure t
  :config
  (progn
    ;; Rebind Shift-SpaceBar to move to a parent directory using the same buffer.
    (add-hook 'dired-mode-hook
              (lambda ()
                (define-key dired-mode-map (kbd "S-<SPC>")
                  (lambda () (interactive) (find-alternate-file "..")))))
    ;; Show files and directories details
    (setq diredp-hide-details-initially-flag nil
          diredp-hide-details-propagate-flag nil)
    ;; Always use one buffer when I move directory
    (declare-function toggle-diredp-find-file-reuse-dir "dired+")
    (toggle-diredp-find-file-reuse-dir 1))
  ;; Dired+ requires ls-lisp
  (use-package ls-lisp
    :config
    (progn
      (setq ls-lisp-emulation 'MS-Windows)
      (ls-lisp-set-options)))
  )

(use-package elmacro
  :disabled nil
  :commands (elmacro-mode)
  :ensure t)

(declare-function eshell "eshell")
(use-package eshell
  :disabled nil
  :commands (eshell)
  :init
  (progn (setq eshell-ask-to-save-history 'always
               eshell-history-size 1000
               eshell-ask-to-save-history 'always
               eshell-cmpl-cycle-completions nil
               eshell-ls-dired-initial-args '("-h")
               eshell-ls-initial-args "-h"
               eshell-ls-use-in-dired t
               eshell-output-filter-functions '(eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom)
               eshell-scroll-show-maximum-output t
               eshell-scroll-to-bottom-on-output t
               eshell-glob-include-dot-dot nil
               eshell-directory-name (concat dotfiles-dir "eshell"))))

;; Load evil-header before evil to make sure all  the binds work everywhere
(use-package evil-leader
  :disabled nil
  :ensure t
  :defer t
  :commands (evil-mode turn-on-evil-mode)
  :init
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ".")
    (evil-leader/set-key
      "." 'evil-repeat
      "RET" 'delete-other-windows
      "SPC" 'ace-jump-word-mode
      "0" 'delete-window
      "1" 'delete-other-windows
      "2" '(lambda () (interactive)(split-window-vertically)(other-window 1))
      "3" '(lambda () (interactive)(split-window-horizontally)(other-window 1))
      "a" 'align-regexp
      "A" 'org-agenda
      "b" 'helm-buffers-list
      "B" 'helm-bookmarks
      "a" 'helm-ack
      "c" 'ace-jump-char-mode
      "C" 'cleanup-buffer
      "d" 'md/duplicate-down
      "D" 'dired-jump
      "e" 'helm-find-files
      "E" 'eval-buffer
      "i" 'helm-imenu
      "g" 'rgrep
      "G" 'helm-google-suggest
      "h" 'helm-apropos
      "k" 'kill-buffer
      "l" 'ace-jump-line-mode
      "o" 'other-window
      "O" 'helm-occur
      "q" 'org-set-tags-command
      "r" 'helm-recentf
      "s" 'smart-switch-to-previous-buffer
      "t" 'comment-dwim-line
      "T" 'org-capture
      "x" 'helm-M-x
      "w" 'save-buffer
      "W" 'whack-whitespace)))

(use-package evil
  :disabled nil
  :commands (evil-mode turn-on-evil-mode)
  :init
  (progn
    (evil-mode 1))
  :config
  (progn
    (global-set-key (kbd "M-SPC") 'evil-normal-state)
    ;; esc key quits everything
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (use-package evil-matchit
      :disabled nil
      :ensure t
      :commands (evil-matchit-mode)
      :init (add-hook 'web-mode-hook 'evil-matchit-mode))
    (use-package evil-nerd-commenter
      :disabled nil
      :ensure t
      :init (evilnc-default-hotkeys))
    (use-package evil-numbers
      :disabled nil
      :ensure t
      :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
      :init
      (progn
        (progn
          (global-set-key [(meta shift up)]   'evil-numbers/inc-at-pt)
          (global-set-key [(meta shift down)] 'evil-numbers/dec-at-pt))))
    (use-package evil-surround
      :disabled nil
      :ensure t
      :defer t
      :init
      (progn
        (global-evil-surround-mode 1)))
    ;; (add-to-list 'surround-operator-alist
    ;;              '(evil-paredit-change . change))
    ;; (add-to-list 'surround-operator-alist
    ;;              '(evil-paredit-delete . delete))))
    (use-package powerline
      :disabled nil
      :ensure t)
    (use-package powerline-evil
      :disabled nil
      :ensure t
      :commands (powerline-evil-vim-color-theme)
      :init
      (progn
        (powerline-evil-vim-color-theme)
        (set-face-background 'powerline-evil-normal-face "dark green")
        (set-face-background 'powerline-evil-insert-face "royal blue")
        (set-face-background 'powerline-evil-visual-face "dark orange")
        (set-face-background 'powerline-evil-operator-face "dark cyan")
        (set-face-background 'powerline-evil-replace-face "dark red")
        (set-face-background 'powerline-evil-motion-face "dark magenta")
        (set-face-background 'powerline-evil-emacs-face "dark violet")))))

(use-package expand-region
  :disabled nil
  :ensure t
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))

(use-package fci
  :disabled t
  :config
  (progn
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
       fundamental-mode-hook))))

(use-package flycheck
  :disabled nil
  :ensure t
  :defer t
  :commands (flycheck-mode)
  :init
  (progn
    (eval-after-load 'flycheck '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (add-hook 'text-mode-hook 'flycheck-mode)))

(use-package fold
  :disabled t
  :init
  (progn
    (add-hook 'sgml-mode-hook 'fold-mode)
    (add-hook 'nxml-mode-hook 'fold-mode)
    (add-hook 'css-mode-hook 'fold-mode)
    (add-hook 'emacs-lisp-mode-hook 'fold-mode)))

;; Loading Helm like :init (helm-mode) adds 5 seconds to the start up time
(use-package helm
  :disabled nil
  :ensure t
  :config
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 20
          helm-ff-skip-boring-files t
          helm-idle-delay 0.0
          helm-quick-update t
          helm-buffers-fuzzy-matching t)
    (helm-match-plugin-mode)
    (setq helm-boring-file-regexp-list
          '("\\.jpg$" "\\.jpeg$" "\\.gif$" "\\.png$" "\\.swf$" "\\.sa$" "\\.fla$" ))
    (bind-key "C-w" 'helm-find-files-up-one-level helm-map)
    (bind-key "C-v" 'helm-execute-persistent-action helm-map))
  :bind (("C-x f" . helm-recentf)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-c h" . helm-mini)
         ("C-c o" . helm-occur)))

(use-package helm-swoop
  :disabled nil
  :ensure t
  :defer t
  :commands (helm-swoop helm-swoop-from-isearch helm-swoop-back-to-last-point)
  :init
  (progn  (bind-key "M-i" 'helm-swoop-from-isearch)
          ;; (bind-key "M-I" 'helm-swoop-back-to-last-point)
          (bind-key "M-I" 'helm-swoop)))

(use-package helm-descbinds
  :disabled nil
  :ensure t
  :defer t
  :commands (helm-descbinds)
  :init
  (progn
    (helm-descbinds-mode)
    (setq helm-descbinds-window-style 'split-window))
  :bind ("C-x b" . helm-descbinds))

(use-package ido
  :disabled nil
  :ensure t
  :defer t
  :commands (ido-mode)
  :config
  (progn
    (setq
     ;; Create a new buffer if no buffer matches substring
     ido-create-new-buffer 'always
     ;; Enable fuzzy search
     ido-enable-flex-matching t
     ;; Uses ido everywhere
     ido-everywhere t
     ;; Enphasize files with the specified extensions
     ido-file-extensions-order '(".xml" ".html" ".css" ".ps1" ".sh" ".el" ".php" ".org")
     ;; Don't highlight first match
     ido-use-faces nil
     ;; Find file at point using ido
     ido-use-filename-at-point 'guess)
    (use-package ido-vertical-mode
      :ensure t
      :config (ido-vertical-mode))
    ;; Use flx-ido for better flex matching between words
    (use-package flx-ido
      :ensure t
      :config
      (flx-ido-mode 1))))

(use-package linum-off
  :disabled nil
  )

(use-package move-dup
  :disabled nil
  :ensure t)

(use-package multiple-cursors
  :disabled nil
  :ensure t
  :defer t
  :config (progn (defvar mc/list-file "~/misc/.mc-lists.el"))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-'" . mc/mark-all-like-this)
         ("C-}" . mc/edit-ends-of-lines)
         ("C-{" . mc/edit-beginnings-of-lines)))

(use-package nxml-mode
  :disabled nil
  :mode (("\\.xml\\'" . nxml-mode))
  :init
  (add-hook 'nxml-mode-hook
            '(lambda()
               (setq nxml-child-indent 4)
               (setq indent-tabs-mode nil))))

(use-package org
  :disabled nil
  :ensure t
  :defer t
  :commands (org-mode)
  :config
  (progn
    (require 'org-agenda)
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (setq org-log-done t)
    ;; Remove 'validate XHTML' link at the bottom
    (setq org-export-html-postamble nil)
    (setq org-agenda-start-with-log-mode t)
    (setq org-tags-column -130)
    (setq org-agenda-tags-column -130)
    (setq org-agenda-window-setup 'current-window)
    (cond
     ((string-equal system-type "gnu/linux") ; linux
      (progn
        (defvar orgfiles-dir "/media/sf_c/Users/Rafael/AppData/Roaming/org/"
          "The root dir of my emacs files in Linux.")
        ;;    (defvar elpa-dir "~/.emacs.d/elpa/"
        ;;      "The directory for elpa packages in Linux.")
        ))
     ((string-equal system-type "windows-nt") ; Microsoft Windows
      (progn
        (defvar orgfiles-dir "~/org/"
          "The root dir of my emacs files in Windows.")
        ;;    (defvar elpa-dir (expand-file-name "elpa/" dotfiles-dir)
        ;;      "The directory for elpa packages in Windows.")
        )))
    (setq org-agenda-files (list
                            (concat orgfiles-dir "agenda.org")
                            (concat orgfiles-dir "gforces.org"))))
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))


(use-package php-mode
  :disabled nil
  :ensure t
  :commands php-mode
  :mode (("\\.php\\'" . php-mode))
  )

(use-package php+-mode
  :disabled t
  :init (php+-mode-setup))

(use-package powershell
  :disabled t
  :ensure t
  :commands (powershell-mode)
  :config     (push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist))

(use-package powershell-mode
  :disabled t
  :ensure t
  :commands (powershell-mode)
  :config
  (progn
    (autoload 'powershell-mode "powershell-mode" "Mode PowerShell" t)
    (push '("\\.ps[12]?$" . powershell-mode) auto-mode-alist)))

(use-package rainbow-mode
  :disabled t
  :init
  (hook-into-modes #'rainbow-mode '(css-mode-hook
                                    emacs-lisp-mode-hook
                                    html-mode-hook
                                    php-mode-hook
                                    prog-mode-hook
                                    python-mode-hook
                                    sgml-mode-hook
                                    xml-mode-hook)))

(use-package rotate-text
  :disabled nil
  :init
  (progn
    (defvar rotate-text-words '(("width" "height")
                                ("plugin" "hotspot")
                                ("yes" "no")
                                ("true" "false")
                                ("left" "right" "top" "bottom"))))
  :bind ("C-c C-c" . rotate-text))

(use-package smex
  :disabled nil
  :ensure t
  :commands (smex smex-major-mode-commands)
  :defer t
  :config
  (progn
    ;; Put smex file in misc folder
    (setq smex-save-file (concat dotfiles-dir "misc/.smex-items")))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package undo-tree
  :disabled nil
  :ensure t
  :commands (undo-tree-mode)
  :config (global-undo-tree-mode))

(use-package volatile-highlights
  :disabled t
  :ensure t
  :commands (volatile-highlights-mode))

(use-package web-mode
  :disabled nil
  :ensure t
  ;; :mode "\\.\\(erb\\|html?\\)\\'"
  ;; :mode (("\\.html\\'" . web-mode))
  :init
  (hook-into-modes #'(lambda () (web-mode ))
                   '(css-mode-hook
                     html-mode-hook))
  :defer t
  :config
  (progn
    (setq web-mode-markup-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-enable-css-colorization t)
    (add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)
                                 (yas-activate-extra-mode 'css-mode)
                                 (evil-leader/set-key "t" 'web-mode-comment-or-uncomment)))))

(use-package yasnippet
  :disabled nil
  :ensure t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(css-mode-hook
                     html-mode-hook
                     message-mode-hook
                     nxml-mode-hook
                     org-mode-hook
                     php-mode-hook
                     prog-mode-hook
                     python-mode-hook
                     sgml-mode-hook
                     xml-mode-hook))
  :config
  (progn
    (yas-load-directory (expand-file-name "snippets/" dotfiles-dir))
    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap))
  :bind ("C-c SPC" . yas-expand))

(provide 'use-packages)
