 ;;; krpano-mode.el --- Major mode for editing Krpano source code
;; Copyright (C) 2015 Rafael Guerra Paz
;; Version: 0.1
;; Keywords: languages
;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-krpano-mode")
;; (require 'krpano-mode)
;;; Commentary:
;; This is the official Emacs mode for editing Krpano programs.
;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;; Code:
(defvar kr-mode-hook nil)
;;;###autoload
;;(add-to-list 'auto-mode-alist '("\\.xml\\'" . kr-mode))

;; Higlight comments and strings

(defconst my-kr-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ' is a string delimiter
    (modify-syntax-entry ?' "\"" table)
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode my-kr-mode prog-mode "Simple Krpano Mode"
  :syntax-table my-kr-mode-syntax-table
  (font-lock-fontify-buffer))