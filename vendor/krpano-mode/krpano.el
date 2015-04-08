;; krpano-mode.el --- Major mode for editing Krpano configuration files
;; Author: Rafael Guerra Paz
;; Keywords: languages
;;; Commentary:
;;
;; This krpano-mode is mainly derived from nxml-mode.
;;; History:
;;
(require 'nxml-mode)
;;; Code:
(defgroup krpano nil
  "Customizations for `krpano-mode'."
  :prefix "krpano-"
  :group 'krpano)

(defvar krpano-mode-hook nil
  "List of functions to be executed on entry to `krpano-mode'.")

(defvar krpano-keywords
  '(;; include
    "\\(" "\\)"
    ;; layer
    "align" "alpha" "alturl" "bgalpha" "bgborder" "bgcapture" "bgcolor" "bgroundedge" "bgshadow" "crop" "devices" "edge" "enabled" "handcursor" "height" "keep" "maskchildren" "name" "onclick" "ondown" "ondowncrop" "onhover" "onloaded" "onout" "onover" "onovercrop" "onup" "parent" "rotate" "scale" "scalechildren" "style" "type" "url" "visible" "width" "x" "y" "zorder"
    ;; hotspot
    "ath" "atv" "borderwidth" "bordercolor" "borderalpha" "children" "distorted" "ox" "oy" "rx" "ry" "rz" "zoom"
    ;; textstyle
    "background" "backgroundcolor" "blendmode" "bold" "border" "bordercolor" "css" "effect" "fadeintime" "fadetime" "font" "fontsize" "html" "italic" "noclip" "origin" "roundedge" "selectable" "showtime" "textalign" "textcolor" "xoffset" "yoffset" "vcenter" "wordwrap"
    ;; data
    "contet"
    ))

(defvar krpano-events
  '(;; Programming
    "asyncfor" "asyncloop" "breakall" "callwith" "copy" "delayedcall" "delete" "events" "for" "get" "if" "ifnot" "loop" "pop" "push" "resolvecondition" "set" "stopall" "stopdelayedcall" "switch"
    ;;Math operators
    "Math.abs" "Math.acos" "Math.asin" "Math.atan" "Math.atan2" "Math.ceil" "Math.cos" "Math.exp" "Math.floor" "Math.log" "Math.max" "Math.min" "Math.pow" "Math.round" "Math.sin" "Math.sqrt" "Math.tan" "add" "clamp" "dec" "div" "inc" "mod" "mul" "pow" "sub"
    ;;Number / String formating"
    "escape" "fromcharcode" "indexoftxt" "roundval" "subtxt" "tohex" "tolower" "toupper" "txtadd" "txtreplace" "unscape"
    ;; Animations / Animated value changing
    "stoptween" "tween types" "tween"
    ;; Dynamic loading of other panos / tours
    "loadpano" "loadscene" "loadxml" "openurl"
    ;; Viewing animations / Camera control
    "adjusthlookat" "getlooktodistance" "lookat" "lookto" "looktohotspot" "moveto" "oninterrupt" "stoplookto" "stopmovements" "wait" "zoomto"
    ;; Coordinate transformation
    "layertoscreen" "remapfovtype" "screentolayer" "screentosphere" "spheretoscreen"
    ;; Text visualization
    "showtext"
    ;; View / Pano updates
    "invalidatescreen" "updateobject" "updatescreen"
    ;; Dynamic adding or removing of screen elements
    "addhotspot" "addlayer" "addlensflare" "addplugin" "removehotspot" "removelayer" "removelensflare" "removeplugin"
    ;; Layer / Plugin / Hotspot Actions
    "changeorigin" "getcenter" "getfullpath" "loadstyle" "registercontentsize" "resetsize" "updatepos"
    ;; External / Javascript Interface
    "fscommand" "js"
    ;; Debugging
    "error" "showlog" "trace"
    ))

(defvar krpano-font-lock-keywords
  (append
   sgml-font-lock-keywords
   `(
     ;; stuff between "
     ("\\[\\(.*?\\)\\]" . font-lock-variable-name-face)
     ;;(".\\|,\\|;\\|(\\|)\\|[\\|]\\|=" . font-lock-keyword-face)
     ;;("\"[^\"]+\"" . font-lock-string-face)
     ( ,(regexp-opt krpano-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt krpano-events 'words) . font-lock-constant-face)
     )))

;;;###autoload
(define-derived-mode krpano-mode sgml-mode "krpano"
  "Major mode for editing Krpano configuration files (.xml)."
  :group 'krpano
  ;; it mainly from nxml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '(krpano-font-lock-keywords
         nil nil nil nil
         (syntax-propertize-function
          . nxml-font-lock-keywords))))

(add-hook 'krpano-mode-hook (lambda () (setq indent-tabs-mode nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.xml$" . krpano-mode))

;; This part ends here
(provide 'krpano-mode)