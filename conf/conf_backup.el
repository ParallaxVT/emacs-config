(defvar user-temporary-backup-directory (concat dotfiles-dir "backup")
  "Directory to store all the backup files")

(make-directory user-temporary-backup-directory t) ; Create directory if doesn't exists already

(setq
 backup-by-copying t                       ; always use copying to create backup files
 backup-directory-alist                    ; alist of filename patterns and backup directory names
 `(("." . ,user-temporary-backup-directory)) ; (REGEXP . DIRECTORY)
 delete-old-versions t                     ; delete excess backup versions siletly
 kept-new-versions 6                       ; number of oldest versions to keep when a new numbered backup is made
 kept-old-versions 2                       ; number of newest versions to keep when a new numbered backup is made
 version-control t                         ; make numeric backup versions
 )

(provide 'conf_backup)