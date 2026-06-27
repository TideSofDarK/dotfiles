;;; gdscript-ts-mode.el --- tree-sitter support for GDScript  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TideS

;; Author           : TideS <tidesmain@gmail.com>
;; Maintainer       : TideS <tidesmain@gmail.com>
;; Created          : January 2026
;; Keywords         : gdscript languages tree-sitter
;; Package-Requires : ((emacs "31.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(eval-when-compile (require 'rx))
(require 'c-ts-common)
(treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
 t)

(defcustom gdscript-ts-indent-offset 4
  "Number of spaces for each indentation step in `gdscript-ts-mode'."
  :version "31.1"
  :type 'integer
  :safe 'integerp
  :group 'gdscript)

(defvar gdscript-ts--keyword-regex
  (rx bot (| "func" "var" "const" "set" "get" "setget" "signal" "extends"
             "match" "if" "elif" "else" "for" "in" "while" "break" "continue"
             "pass" "return" "when" "yield" "await"
             "class" "class_name" "abstract" "is" "onready" "tool" "static"
             "export" "as" "void" "enum" "assert" "breakpoint"
             "sync" "remote" "master" "puppet"
             "remotesync" "mastersync" "puppetsync"
             "trait" "namespace" "super"
             "and" "or" "not"
             "await" "yield" "self")
    eot))

(defvar gdscript-ts--builtin-type-regex
  "\\`\\(int\\|bool\\|float\\|void\\|Vector2\\|Vector2i\\|Vector3\\|Vector3i\\|Vector4\\|Vector4i\\|Color\\|Rect2\\|Rect2i\\|Array\\|Basis\\|Dictionary\\|Plane\\|Quat\\|RID\\|Rect3\\|Transform\\|Transform2D\\|Transform3D\\|AABB\\|String\\|Color\\|NodePath\\|PoolByteArray\\|PoolIntArray\\|PoolRealArray\\|PoolStringArray\\|PoolVector2Array\\|PoolVector3Array\\|PoolColorArray\\|bool\\|int\\|float\\|Signal\\|Callable\\|StringName\\|Quaternion\\|Projection\\|PackedByteArray\\|PackedInt32Array\\|PackedInt64Array\\|PackedFloat32Array\\|PackedFloat64Array\\|PackedStringArray\\|PackedVector2Array\\|PackedVector2iArray\\|PackedVector3Array\\|PackedVector3iArray\\|PackedVector4Array\\|PackedColorArray\\|JSON\\|UPNP\\|OS\\|IP\\|JSONRPC\\|XRVRS\\)\\'")

(defvar gdscript-ts--type-regex
  "\\`[A-Z][a-zA-Z0-9_]*[a-z][a-zA-Z0-9_]*\\'")

(defvar gdscript-ts--constant-regex "\\`[A-Z_][A-Z0-9_]+\\'")

(defvar gdscript-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'gdscript
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'gdscript
   :feature 'constant
   `(([(null) (false) (true)] @font-lock-constant-face)
     (const_statement name: (name) @font-lock-constant-face)
     (enumerator left: (identifier) @font-lock-constant-face)
     ((identifier) @font-lock-constant-face
      (:match ,gdscript-ts--constant-regex @font-lock-constant-face))
     (variable_statement
      name: (name) @font-lock-constant-face
      (:match ,gdscript-ts--constant-regex @font-lock-constant-face)))

   :language 'gdscript
   :feature 'bracket
   `(["[" "]" "(" ")" "{" "}"] @font-lock-bracket-face)

   :language 'gdscript
   :feature 'delimiter
   `(["," ":" "."] @font-lock-delimiter-face)

   :language 'gdscript
   :feature 'type
   `(((identifier) @font-lock-builtin-face
      (:match ,gdscript-ts--builtin-type-regex @font-lock-builtin-face))
     (get_node) @font-lock-builtin-face
     ((identifier) @font-lock-type-face
      (:match ,gdscript-ts--type-regex @font-lock-type-face))
     (enum_definition name: (_) @font-lock-type-face)
     (class_name_statement (name) @font-lock-type-face)
     (class_definition (name) @font-lock-type-face))

   :language 'gdscript
   :feature 'definition
   '((function_definition (name) @font-lock-function-name-face))

   :language 'gdscript
   :feature 'annotation
   '((annotation "@" @font-lock-preprocessor-face
                 (identifier) @font-lock-preprocessor-face))

   :language 'gdscript
   :feature 'keyword
   `((ERROR _ @font-lock-keyword-face (:match ,gdscript-ts--keyword-regex @font-lock-keyword-face))
     (_ _ @font-lock-keyword-face (:match ,gdscript-ts--keyword-regex @font-lock-keyword-face)))

   :language 'gdscript
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'gdscript
   :feature 'function
   '((call (identifier) @font-lock-builtin-face (:match "preload" @font-lock-builtin-face))
     (call (identifier) @font-lock-function-call-face)
     (attribute_call (identifier) @font-lock-function-call-face))

   :language 'gdscript
   :feature 'number
   '(([(integer) (float)] @font-lock-number-face))

   :language 'gdscript
   :feature 'property
   '((attribute (identifier) (identifier) @font-lock-property-use-face))

   :feature 'operator
   :language 'gdscript
   `(["+" "+="   "-" "-=" "*" "*=" "/" "/=" "^"  "^="  ">"  ">="
      "<" "<="   "|" "|=" "%" "%=" "&" "&=" ">>" ">>=" "<<" "<<="
      "||" "&&" "==" "!=" "->" "~" "=" ":="]
     @font-lock-operator-face)

   :language 'gdscript
   :override t
   :feature 'escape-sequence
   '((escape_sequence) @font-lock-escape-face)))

(defvar gdscript-ts-mode--simple-indent-rules
  `((gdscript
     ((parent-is "block") parent 0)
     ((parent-is "chunk") column-0 0)
     ((parent-is "ERROR") no-indent 0)
     c-ts-common-baseline-indent-rule)))

;;;###autoload
(define-derived-mode gdscript-ts-mode prog-mode "GDScript"
  "Major mode for editing GDScript, powered by tree-sitter.

Since this mode uses a parser, unbalanced brackets might cause
some breakage in indentation/fontification.  Therefore, it's
recommended to enable `electric-pair-mode' with this mode."
  :group 'gdscript

  (when (and (treesit-ready-p 'gdscript t)
             (treesit-ensure-installed 'gdscript))
    (treesit-parser-create 'gdscript)

    (setq-local c-ts-common-indent-offset 'gdscript-ts-indent-offset)
    (setq-local c-ts-common-list-indent-style 'simple)
    (setq-local treesit-simple-indent-rules gdscript-ts-mode--simple-indent-rules)

    ;; (setq-local electric-indent-chars
    ;;             (append ":" electric-indent-chars))

    (setq-local treesit-font-lock-settings gdscript-ts--treesit-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type annotation)
                  ( number constant escape-sequence)
                  ( bracket delimiter function operator property)))

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'gdscript-ts-mode '(gdscript-mode))

;;;###autoload
(defun gdscript-ts-mode-maybe ()
  "Enable `gdscript-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'gdscript)
          (eq treesit-enabled-modes t)
          (memq 'gdscript-ts-mode treesit-enabled-modes))
      (gdscript-ts-mode)
    (prog-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist
               '("\\(?:.gd\\)\\'" . gdscript-ts-mode-maybe))
  ;; To be able to toggle between an external package and this ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(gdscript-mode . gdscript-ts-mode)))

(provide 'gdscript-ts-mode)

;;; gdscript-ts-mode.el ends here
