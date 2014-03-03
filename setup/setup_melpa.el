;; ====================
;; setup_melpa.el
;; ====================

;; Set elpa directory before any (require 'package)
(setq package-user-dir (concat dotfiles-dir "elpa"))

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;           '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar packages-list
  '(dired+ dropdown-list evil-numbers expand-region fill-column-indicator flycheck helm magit mmm-mode php+-mode powershell-mode rainbow-mode smex undo-tree volatile-highlights yasnippet)
  "A list of packages to ensure are installed at launch.")

(defun check-packages-installed ()
  "Check if all packages in `packages-list' are installed."
  (every #'package-installed-p packages-list))

(defun install-packages ()
  "Install all packages listed in `packages-list'."
  (unless (check-packages-installed)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
          (remove-if #'package-installed-p packages-list))))

(install-packages)

(provide 'setup_melpa)