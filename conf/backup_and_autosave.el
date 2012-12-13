;; Emacs configuration file


(defvar user-temporary-file-directory "~/.emacs/backup_and_autosave")

(make-directory user-temporary-file-directory t) ; Create directory if doesn't exists already

;;; ========================================
;;; backup
;;; ========================================

(setq
 backup-by-copying t                       ; always use copying to create backup files
 backup-directory-alist                    ; alist of filename patterns and backup directory names
 '(("." . ,user-temporary-file-directory)) ; (REGEXP . DIRECTORY)
 delete-old-versions t                     ; delete excess backup versions siletly
 kept-new-versions 6                       ; number of oldest versions to keep when a new numbered backup is made
 kept-old-versions 2                       ; number of newest versions to keep when a new numbered backup is made
 version-control t                         ; make numeric backup versions
 )

;;; ========================================
;;; auto-save
;;; ========================================

(setq
 auto-save-file-name-transforsm            ; place all auto-save files in a dedicated directory
 '((".*" ,user-temporary-file-directory t))
 )

;;; ========================================
;;; misc
;;; ========================================

(global-auto-revert-mode t)                ; revert buffers every 5 seconds


