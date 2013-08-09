;; ====================
;; mod_org.el
;; ====================

;;; remove 'validate XHTML' link at the bottom
(setq org-export-html-postamble nil)

(require 'org-publish)
(setq org-publish-project-alist
      '(
        ("B-inherit"
         :base-directory "C:/Users/rafaelgp/AppData/Roaming/org/"
         :recursive t
         :base-extension "css\\|js"
         :publishing-directory "C:/Users/rafaelgp/AppData/Roaming/public_html/"
         :publishing-function org-publish-attachment
         )
        ("org-notes"
         :base-directory "C:/Users/rafaelgp/AppData/Roaming/org/"
         :base-extension "org"
         :publishing-directory "C:/Users/rafaelgp/AppData/Roaming/public_html/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4             ; Just the default for this project.
         ;; :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/style.css\" />"
         :auto-preamble n
         :auto-sitemap t                ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
         :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
         :sitemap-style "tree"
         )
        ("org-static"
         :base-directory "C:/Users/rafaelgp/AppData/Roaming/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "C:/Users/rafaelgp/AppData/Roaming/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("org" :components ("org-notes" "org-static"))
        ))

(provide 'mod_org)