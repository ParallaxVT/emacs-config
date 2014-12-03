;; autocompile
;; =================================================================

(defun autocompile nil
  "Compile if init.el is newer than init.elc"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs.d/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
        (byte-compile-file dotemacs)))
  (let ((usepackage (expand-file-name "~/.emacs.d/setup/use-packages.el")))
    (if (string= (buffer-file-name) (file-chase-links usepackage))
        (byte-compile-file usepackage)))
  (let ((defunfile (expand-file-name "~/.emacs.d/setup/setup_defun.el")))
    (if (string= (buffer-file-name) (file-chase-links defunfile))
        (byte-compile-file defunfile))))

;; cleanup_buffer
;; ====================================================================

;; Indent current buffer
(defun indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
;; Replace tabs with spaces
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (whitespace-cleanup)
  (message "Buffer cleaned") )

;; comment_line
;; ==================================================================

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; indent_region_or_buffer
;; ==================================================================

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

;; kill_whole-line
;; =================================================================

(defun smart-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

;; kill_region
;; =================================================================

;; The equivalent of 'dd' or 'D' in Vim
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; kill_ring_save
;; ====================================================================

;; The equivalent of 'yy' or 'Y' in Vim
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; eshell/clear
;; ==================================================================

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; kill scratch buffer
;; ==================================================================

;; If the *scratch* buffer is killed, recreate it automatically
(with-current-buffer (get-buffer-create "*scratch*"))
(lisp-interaction-mode)
(make-local-variable 'kill-buffer-query-functions)
(add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (princ ";; Scratch Buffer" (get-buffer "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;; Swap windows
;; ==================================================================

(defun smart-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

;; Switch to previous buffer
;; ==================================================================

(defun smart-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; kill other buffers
;; ==================================================================

(defun smart-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
      (->> (buffer-list)
        (-filter #'buffer-file-name)
        (--remove (eql (current-buffer) it)))
    #'kill-buffer))

;; Create scratch buffer
;; ==================================================================

(defun smart-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))

;; Move cursor to beginning of line
;; ==================================================================

(defun smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smart-move-beginning-of-line)

;; Open new lines
;; ==================================================================

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (forward-line -1)
  (smart-open-line))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Change file coding system
;; ==================================================================

(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

;; Search
;; ==================================================================

;; Center screen when perform a search
(defadvice
  isearch-forward
  (after isearch-forward-recenter activate)
  (recenter))
(ad-activate 'isearch-forward)

(defadvice
  isearch-repeat-forward
  (after isearch-repeat-forward-recenter activate)
  (recenter))
(ad-activate 'isearch-repeat-forward)

(defadvice
  isearch-repeat-backward
  (after isearch-repeat-backward-recenter activate)
  (recenter))
(ad-activate 'isearch-repeat-backward)

;; Rgrep
;; ==================================================================

;; Delete the first 4 lines in the rgrep outut buffer
;; Type (C-x n w) to reveal this lines
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;; Text Editing
;; ==================================================================

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    ;; (replace-match "" nil nil)
    (fixup-whitespace)
    (evil-forward-char))
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (replace-match "_" nil nil)))

;; Cusmtom macros
;; =================================================================

(defun gforces-config ()
  "Change me!"
  (interactive)
  (evil-beginning-of-line)
  (md/duplicate-down 1)
  (evil-insert 1 nil nil)
  (indent-for-tab-command nil)
  (evil-normal-state 1)
  (evil-previous-line nil)
  (evil-first-non-blank)
  (evil-insert 1 nil nil)
  (insert "<car id=\"")
  (evil-append-line 1)
  (insert "\"")
  (evil-normal-state 1)
  (evil-next-line nil)
  (evil-beginning-of-line)
  (delete-char 3)
  (capitalize-word 1)
  (let ((end (copy-marker (line-end-position))))
    (while (re-search-forward "_" end t)
      (replace-match " " nil nil)
      (capitalize-word 1)))
  (evil-end-of-line 1)
  (backward-char 4)
  (delete-char 1)
  (insert " - ")
  (evil-normal-state 1)
  (evil-first-non-blank)
  (evil-insert 1 nil nil)
  (insert "name=\"")
  (evil-normal-state 1)
  (evil-append-line 1)
  (insert "\" h=\"\" v=\"\" \/>")
  (evil-normal-state 1)
  (join-line)
  (evil-next-line nil)
  )

(provide 'setup_defun)