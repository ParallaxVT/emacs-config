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
       (sw-red-1	"#e5786d")
       (sw-red		"#95e454")
       (sw-blue-2	"#2e3436")
       (sw-blue-1	"#64a8d8")
       (sw-blue		"#8ac6f2")
       (sw-blue-1       "#5fd7ff")
       (sw-blue-2       "#d7ffff")
       (sw-magenta	"#cc99cc")
       (sw-orange-1	"#f57900")
       (sw-orange	"#e65c00")
       (sw-orange+1	"#e9b96e")
       (sw-orange+2	"#ffc125")
       (sw-purple-1	"#ad7fa8")
       (sw-purple	"#cc99cc")
       (sw-pink-1	"#f283b6")
       (sw-pink		"#f6b3df")
       (sw-gray-1	"#444444")
       (sw-gray		"#424242")
       (sw-gray+1	"#99968b"))

(custom-theme-set-faces
 'super-wombat
 
`(default ((t (:foreground ,sw-fg :background ,sw-bg))))
`(highlight ((t (:background ,sw-blue))))
`(region ((t (:foreground ,sw-fg :background ,sw-gray-1))))

;; Font Lock
`(font-lock-builtin-face ((t (:foreground ,sw-blue))))
`(font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground ,sw-gray+1))))
`(font-lock-comment-face ((t (:italic t :slant italic :foreground ,sw-gray+1))))
`(font-lock-constant-face ((t (:foreground ,sw-red-1))))
`(font-lock-doc-face ((t (:foreground ,sw-gray+1))))
`(font-lock-function-name-face ((t (:foreground ,sw-purple-1 :bold t :italic t))))
`(font-lock-keyword-face ((t (:foreground ,sw-blue))))
`(font-lock-negation-char-face ((t (:foreground ,sw-red))))
`(font-lock-preprocessor-face ((t (:foreground ,sw-red-1))))
`(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
`(font-lock-regexp-grouping-construct ((t (:bold t ,sw-green))))
`(font-lock-string-face ((t (:italic t :foreground ,sw-green))))
`(font-lock-type-face ((t (:foreground ,sw-green+1))))
`(font-lock-variable-name-face ((t (:foreground ,sw-blue))))
`(font-lock-warning-face ((t (:bold t :foreground ,sw-red))))

;; UI Items
`(minibuffer-prompt ((t (:foreground ,sw-red :bold t))))
`(mode-line ((t (:background ,sw-gray-1 :foreground ,sw-fg))))
`(mode-line-emphasis ((t (:bold t))))
`(mode-line-highlight ((t (:background ,sw-orange :box nil))))
`(mode-line-inactive ((t (:background ,sw-bg :box (:line-width 1 :color ,sw-gray :style nil)))))
`(region ((t (:foreground ,sw-fg :background ,sw-gray-1))))

;; Org-mode
`(org-date ((t (:foreground "Cyan" :underline t))))
`(org-agenda-date ((t (:foreground ,sw-blue))))
`(org-agenda-date-weekend ((t (:bold t :foreground ,sw-orange :weight bold))))
`(org-hide ((t (:foreground ,sw-bg))))
`(org-todo ((t (:foreground ,sw-pink :bold t))))
`(org-hide ((t (:foreground ,sw-bg))))
`(org-done ((t (:foreground ,sw-green+2 :bold t))))
`(org-level-1 ((t (:foreground ,sw-blue :bold t))))
`(org-level-2 ((t (:foreground "#ee9a49"))))
`(org-level-3 ((t (:foreground "#ff83fa"))))
`(org-level-4 ((t (:foreground "#ffa500"))))
`(org-level-5 ((t (:foreground "#ff4040"))))

;; isearch
`(isearch ((t (:background ,sw-orange-1 :foreground ,sw-blue-2))))
`(isearch-lazy-highlight-face ((t (:foreground ,sw-blue-2 :background ,sw-orange+1))))

;; Parenthesis Matching
`(paren-face-match ((t (:inherit show-paren-match-face))))
`(paren-face-match-light ((t (:inherit show-paren-match-face))))
`(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
`(show-paren-match-face ((t (:background ,sw-orange :foreground "white" :bold t))))
`(show-paren-mismatch-face ((t (:background ,sw-purple-1 :foreground ,sw-blue-2))))

`(persp-selected-face ((t (:foreground ,sw-blue-2))))

`(info-xref ((t (:foreground ,sw-blue))))
`(info-xref-visited ((t (:foreground ,sw-purple-1))))

;; eshell
`(eshell-prompt ((t (:foreground ,sw-blue-1))))
`(eshell-ls-directory ((t (:foreground ,sw-blue-1 :width normal))))

 )
 )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'super-wombat)

;;; super-wombat-theme.el ends here
