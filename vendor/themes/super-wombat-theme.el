;;; super-wombat-theme.el --- Super Wombat theme for Emacs.

;; Author: Rafael Guerra Paz <mr.rafaelgpe@gmail.com>
;; URL:
;; Version: 0.0.1
;; Package-Requires:

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(deftheme super-wombat "Super Wombat theme for Emacs")

 (let ((sw-fg		"#f6f3e8")
       (sw-bg		"#111111")
       (sw-green	"#95e454")
       (sw-green+1	"#cae682")
       (sw-green+2	"#4bc98a")
       (sw-red	        "#e5786d")
       (sw-blue		"#8ac6f2")
       (sw-blue+1	"#64a8d8")
       (sw-blue+2       "#5fd7ff")
       (sw-blue+3       "#d7ffff")
       (sw-orange	"#e65c00")
       (sw-orange+1	"#f57900")
       (sw-orange+2	"#e9b96e")
       (sw-orange+3	"#ffc125")
       (sw-purple	"#cc99cc")
       (sw-purple+1	"#ad7fa8")
       (sw-pink		"#f6b3df")
       (sw-pink+1	"#f283b6")
       (sw-gray		"#424242")
       (sw-gray+1	"#444444")
       (sw-gray+2	"#99968b"))

(custom-theme-set-faces
 'super-wombat
 
`(default ((t (:foreground ,sw-fg :background ,sw-bg))))
`(highlight ((t (:background ,sw-gray+1))))
`(region ((t (:foreground ,sw-fg :background ,sw-gray+1))))

;; Font Lock
`(font-lock-builtin-face ((t (:foreground ,sw-blue))))
`(font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground ,sw-gray+2))))
`(font-lock-comment-face ((t (:italic t :slant italic :foreground ,sw-gray+2))))
`(font-lock-constant-face ((t (:foreground ,sw-red))))
`(font-lock-doc-face ((t (:foreground ,sw-gray+2))))
`(font-lock-function-name-face ((t (:foreground ,sw-blue :italic t))))
`(font-lock-keyword-face ((t (:foreground ,sw-blue))))
`(font-lock-negation-char-face ((t (:foreground ,sw-green))))
`(font-lock-preprocessor-face ((t (:foreground ,sw-red))))
`(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
`(font-lock-regexp-grouping-construct ((t (:bold t ,sw-green))))
`(font-lock-string-face ((t (:italic t :foreground ,sw-green))))
`(font-lock-type-face ((t (:foreground ,sw-green+1))))
`(font-lock-variable-name-face ((t (:foreground ,sw-green+1))))
`(font-lock-warning-face ((t (:bold t :foreground ,sw-green))))

;; UI Items
`(minibuffer-prompt ((t (:foreground ,sw-green :bold t))))
`(mode-line ((t (:background ,sw-gray+1 :foreground ,sw-fg))))
`(mode-line-emphasis ((t (:bold t))))
`(mode-line-highlight ((t (:background ,sw-orange :box nil))))
`(mode-line-inactive ((t (:background ,sw-bg :box (:line-width 1 :color ,sw-gray :style nil)))))
`(region ((t (:foreground ,sw-fg :background ,sw-gray+1))))

;; Org-mode
`(org-date ((t (:foreground "Cyan" :underline t))))
`(org-agenda-date ((t (:foreground ,sw-blue))))
`(org-agenda-date-weekend ((t (:bold t :foreground ,sw-orange :weight bold))))
`(org-hide ((t (:foreground ,sw-bg))))
`(org-todo ((t (:foreground ,sw-red :bold t))))
`(org-done ((t (:foreground ,sw-green :bold t))))
`(org-level-1 ((t (:foreground "#DFAF8F"))))
`(org-level-2 ((t (:foreground "#BFEBBF"))))
`(org-level-3 ((t (:foreground "#7CB8BB"))))
`(org-level-4 ((t (:foreground "#D0BF8F"))))
`(org-level-5 ((t (:foreground "#93E0E3"))))
`(org-level-6 ((t (:foreground "#9FC59F"))))
`(org-level-7 ((t (:foreground "#8C5353"))))
`(org-level-8 ((t (:foreground "#4C7073"))))

;; isearch
`(isearch ((t (:background ,sw-orange+1 :foreground ,sw-blue+3))))
`(isearch-lazy-highlight-face ((t (:foreground ,sw-blue+3 :background ,sw-orange+2))))

;; Parenthesis Matching
`(paren-face-match ((t (:inherit show-paren-match-face))))
`(paren-face-match-light ((t (:inherit show-paren-match-face))))
`(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
`(show-paren-match-face ((t (:background ,sw-orange :foreground "white" :bold t))))
`(show-paren-mismatch-face ((t (:background ,sw-purple+1 :foreground ,sw-blue+3))))

`(persp-selected-face ((t (:foreground ,sw-blue+3))))

`(info-xref ((t (:foreground ,sw-blue))))
`(info-xref-visited ((t (:foreground ,sw-purple+1))))

;; eshell
`(eshell-prompt ((t (:foreground ,sw-blue+2))))
`(eshell-ls-directory ((t (:foreground ,sw-blue+2 :width normal))))

;; diredplus

`(diredp-dir-priv((t (:foreground ,sw-blue :background "#2b2b2b"))))
`(diredp-dir-heading((t (:foreground ,sw-blue :background "#003f34"))))
`(diredp-no-priv ((t (:foreground ,sw-fg :background "#2b2b2b"))))
`(diredp-read-priv ((t (:foreground ,sw-green :background "#993255"))))
`(diredp-write-priv ((t (:foreground ,sw-green :background "#268f2a"))))
`(diredp-exec-priv ((t (:foreground ,sw-green :background "#4f3c22"))))
`(diredp-number((t (:foreground ,sw-green+1 :background ,sw-bg))))
`(diredp-date-time((t (:foreground ,sw-green+1 :background ,sw-bg))))
`(diredp-file-name((t (:foreground ,sw-green :background ,sw-bg))))
`(diredp-file-suffix((t (:foreground ,sw-green :background ,sw-bg))))
`(diredp-ignored-file-name((t (:foreground ,sw-red :background ,sw-bg))))

;; sh-mode
`(sh-heredoc ((t (:foreground ,sw-fg :bold n :width normal :weight normal))))

 )
 )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'super-wombat)

;;; super-wombat-theme.el ends here

