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

;; (let ((sw-fg            "light yellow")
(let ((sw-fg            "mint cream")
      (sw-bg            "gray7")
      (sw-green         "OliveDrab1")
      (sw-green+1       "OliveDrab3")
      (sw-green+2       "OliveDrab4")
      (sw-red           "indian red")
      (sw-red+1         "Firebrick2")
      (sw-blue-2        "DeepSkyBLue1")
      (sw-blue-1        "SteelBLue1")
      (sw-blue          "sky blue")
      (sw-blue+1        "SkyBlue3")
      (sw-blue+2        "turquoise1")
      (sw-blue+3        "LightCyan1")
      (sw-orange        "tomato")
      (sw-orange+1      "Orange")
      (sw-purple        "plum3")
      (sw-purple+1      "plum4")
      (sw-pink          "maroon")
      (sw-pink+1        "hot pink")
      (sw-pink+3        "deep pink")
      (sw-yellow        "light goldenrod")
      (sw-gray          "gray26")
      (sw-gray+1        "gray27")
      (sw-gray+2        "LavenderBlush4"))

  (custom-theme-set-faces
   'super-wombat

   `(default ((t (:foreground ,sw-fg :background ,sw-bg))))
   `(highlight ((t (:background ,sw-gray+1))))
   `(region ((t (:foreground ,sw-fg :background ,sw-pink))))

   ;; Font Lock
   `(font-lock-builtin-face ((t (:foreground ,sw-blue))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,sw-gray+2))))
   `(font-lock-comment-face ((t (:foreground ,sw-gray+2))))
   `(font-lock-constant-face ((t (:foreground ,sw-red))))
   `(font-lock-doc-face ((t (:foreground ,sw-gray+2))))
   `(font-lock-function-name-face ((t (:foreground ,sw-blue+2))))
   `(font-lock-keyword-face ((t (:foreground ,sw-blue))))
   `(font-lock-negation-char-face ((t (:foreground ,sw-green))))
   `(font-lock-preprocessor-face ((t (:foreground ,sw-red))))
   `(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
   `(font-lock-regexp-grouping-construct ((t (:bold t ,sw-green))))
   `(font-lock-string-face ((t (:foreground ,sw-green))))
   `(font-lock-type-face ((t (:foreground ,sw-green))))
   `(font-lock-variable-name-face ((t (:foreground ,sw-pink+3))))
   `(font-lock-warning-face ((t (:bold t :foreground ,sw-green))))

   ;; nxml

   ;; `(nxml-attribute-local-name ((t (:foreground ,sw-))))
   ;; `(nxml-attribute-value-delimiter ((t (:foreground ,sw-))))
   ;; `(nxml-cdata-section-content ((t (:foreground ,sw-))))
   ;; `(nxml-char-ref-delimiter ((t (:foreground ,sw-))))
   ;; `(nxml-comment-content ((t (:foreground ,sw-))))
   ;; `(nxml-comment-delimiter ((t (:foreground ,sw-))))
   ;; `(nxml-delimited-data ((t (:foreground ,sw-))))
   ;; `(nxml-delimiter ((t (:foreground ,sw-))))
   ;; `(nxml-element-colon ((t (:foreground ,sw-))))
   ;; `(nxml-element-local-name ((t (:foreground ,sw-))))
   ;; `(nxml-element-prefix ((t (:foreground ,sw-))))
   ;; `(nxml-entity-ref-delimiter ((t (:foreground ,sw-))))
   ;; `(nxml-glyph ((t (:foreground ,sw-))))
   ;; `(nxml-hash ((t (:foreground ,sw-))))
   ;; `(nxml-namespace-attribute-value-delimiter ((t (:foreground ,sw-))))
   ;; `(nxml-namespace-attribute-xmlns ((t (:foreground ,sw-))))
   ;; `(nxml-outline-ellipsis ((t (:foreground ,sw-))))
   ;; `(nxml-outline-indicator ((t (:foreground ,sw-))))
   ;; `(nxml-prolog-literal-delimiter ((t (:foreground ,sw-))))
   `(nxml-tag-delimiter ((t (:foreground ,sw-blue+2))))


   ;; UI Items
   `(minibuffer-prompt ((t (:foreground ,sw-green :bold t))))
   `(mode-line ((t (:background ,sw-gray+1 :foreground ,sw-fg))))
   `(mode-line-emphasis ((t (:bold t))))
   `(mode-line-emphasis ((t (:bold t))))
   `(mode-line-highlight ((t (:background ,sw-pink :box nil))))
   `(mode-line-inactive ((t (:background ,sw-bg :box (:line-width 1 :color ,sw-gray :style nil)))))

   ;; Web-mode

   `(web-mode-html-tag-face ((t (:foreground ,sw-blue))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,sw-green))))
   `(web-mode-html-attr-name-face ((t (:foreground ,sw-green))))

   ;; Org-mode
   `(org-date ((t (:foreground ,sw-purple :underline t))))
   `(org-special-keyword ((t (:foreground ,sw-purple))))
   `(org-agenda-date ((t (:foreground ,sw-blue-1))))
   `(org-agenda-date-weekend ((t (:foreground "DeepSkyBLue1"))))
   `(org-agenda-calendar-event ((t (:foreground ,sw-orange+1))))
   `(org-hide ((t (:foreground ,sw-bg))))
   `(org-todo ((t (:foreground ,"Firebrick2" :bold t))))
   `(org-done ((t (:foreground ,"Gray40" :bold t))))
   `(org-level-1 ((t (:foreground ,sw-pink+3))))
   `(org-level-2 ((t (:foreground ,sw-green+1))))
   `(org-level-3 ((t (:foreground ,sw-blue))))
   `(org-level-4 ((t (:foreground ,sw-red))))
   `(org-level-5 ((t (:foreground ,sw-yellow))))
   `(org-level-6 ((t (:foreground ,sw-orange+1))))
   `(org-level-7 ((t (:foreground ,sw-red+1))))
   `(org-level-8 ((t (:foreground ,sw-fg))))
   `(org-block ((t (:foreground "LawnGreen"))))
   `(org-block-begin-line ((t (:foreground "ForestGreen"))))
   `(org-block-end-line ((t (:foreground "ForestGreen"))))
   `(org-agenda-done ((t (:foreground "Gray30"))))
   `(org-scheduled ((t (:foreground ,sw-green+1))))
   `(org-scheduled-today ((t (:foreground ,sw-orange+1))))
   `(org-scheduled-previously ((t (:foreground ,sw-orange+1))))
   `(org-tag ((t (:foreground "SteelBLue1"))))

   ;; isearch
   `(isearch ((t (:background ,sw-pink :foreground ,"white"))))
   `(isearch-lazy-highlight-face ((t (:foreground ,"white" :background ,sw-pink+1))))

   ;; Parenthesis Matching
   `(paren-face-match ((t (:inherit show-paren-match-face))))
   `(paren-face-match-light ((t (:inherit show-paren-match-face))))
   `(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
   `(show-paren-match((t (:background ,sw-pink :foreground "white" :bold t))))
   `(show-paren-mismatch ((t (:background ,sw-purple+1 :foreground ,sw-blue+3))))

   `(persp-selected-face ((t (:foreground ,sw-blue+3))))

   `(info-xref ((t (:foreground ,sw-blue))))
   `(info-xref-visited ((t (:foreground ,sw-purple+1))))

   ;; eshell
   `(eshell-ls-archive-face ((t (:bold t :foreground "IndianRed"))))
   `(eshell-ls-backup-face ((t (:foreground "grey"))))
   `(eshell-ls-clutter-face ((t (:foreground "DimGray"))))
   `(eshell-ls-directory-face ((t (:foreground ,sw-green :width normal))))
   `(eshell-ls-executable-face ((t (:foreground "green"))))
   `(eshell-ls-missing-face ((t (:foreground "black"))))
   `(eshell-ls-picture-face ((t (:foreground "violet"))))
   `(eshell-ls-product-face ((t (:foreground "LightSalmon"))))
   `(eshell-ls-readonly-face ((t (:foreground "aquamarine"))))
   `(eshell-ls-special-face ((t (:foreground "gold"))))
   `(eshell-ls-symlink-face ((t (:foreground "white"))))
   `(eshell-ls-text-face ((t (:foreground "medium aquamarine"))))
   `(eshell-ls-todo-face ((t (:bold t :foreground "aquamarine"))))
   `(eshell-ls-unreadable-face ((t (:foreground "DimGray"))))
   `(eshell-prompt ((t (:foreground  "deep pink"))))
   `(eshell-prompt-face ((t (:foreground  "deep pink"))))
   ;; diredplus

   `(diredp-dir-priv((t (:foreground ,sw-blue :background "#2b2b2b"))))
   `(diredp-dir-heading((t (:foreground ,sw-blue :background "#003f34"))))
   `(diredp-no-priv ((t (:foreground ,sw-fg :background "#2b2b2b"))))
   `(diredp-read-priv ((t (:foreground ,sw-green :background "#993255"))))
   `(diredp-write-priv ((t (:foreground ,sw-green :background "#268f2a"))))
   `(diredp-exec-priv ((t (:foreground ,sw-green :background "#4f3c22"))))
   `(diredp-number((t (:foreground ,sw-green :background ,sw-bg))))
   `(diredp-date-time((t (:foreground ,sw-green :background ,sw-bg))))
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

