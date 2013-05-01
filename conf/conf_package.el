;; ====================
;; conf_package.el
;; ====================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;           '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

