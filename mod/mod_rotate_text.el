;; ====================
;; mod_rotate_text.el
;; ====================

(autoload 'rotate-text "rotate-text" nil t)
(autoload 'rotate-text-backward "rotate-text" nil t)

(defcustom rotate-text-words '(("width" "height")
                               ("plugin" "hotspot")
                               ("yes" "no")
                               ("true" "false")
                               ("left" "right" "top" "bottom"))
  "*List of words to rotate.
Each element is a list of words that should be cycled through.  Individual
segments in symbol names are recognized as words, i.e. windowWidth can be
replaced with windowHeight.
All entries must be in lower case. The case is determined by the rotated
text."
  :group 'rotate-text
  :type '(repeat (repeat :tag "Rotation group" (string :tag "Word"))))

(provide 'mod_rotate_text)