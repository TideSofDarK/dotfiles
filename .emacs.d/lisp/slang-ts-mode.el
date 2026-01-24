;;; c-ts-mode.el --- tree-sitter support for Slang  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TideS

;; Author           : TideS <tidesmain@gmail.com>
;; Maintainer       : TideS <tidesmain@gmail.com>
;; Created          : January 2026
;; Keywords         : slang languages tree-sitter
;; Package-Requires : ((emacs "30.1"))

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

(require 'treesit)
(require 'c-ts-common)
(require 'c-ts-mode)
(eval-when-compile (require 'rx))
;; (treesit-declare-unavailable-functions)

(add-to-list
 'treesit-language-source-alist
 '(slang "https://github.com/tree-sitter-grammars/tree-sitter-slang"
         :commit "1dbcc4abc7b3cdd663eb03d93031167d6ed19f56")
 t)

(defmacro slang-ts--static-if (condition then-form &rest else-forms)
  (declare (indent 2)
           (debug (sexp sexp &rest sexp)))
  (if (eval condition lexical-binding)
      then-form
    (cons 'progn else-forms)))

(defun slang-ts--apply-indent-rules (style)
  (setq-local c-ts-mode-indent-style (or style c-ts-mode-indent-style))
  (setq-local treesit-simple-indent-rules
              (slang-ts--static-if (< emacs-major-version 31)
                                   (c-ts-mode--get-indent-style 'cpp)
                                   (c-ts-mode--simple-indent-rules 'cpp c-ts-mode-indent-style)))
  (setcar (car treesit-simple-indent-rules) 'slang))

(defvar slang-ts-mode--operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++" ".*" "->*" "<=>")
  "Slang operators for tree-sitter font-locking.")

(defvar slang-ts-mode--preproc-keywords
  '("#define" "#if" "#ifdef" "#ifndef"
    "#else" "#elif" "#endif" "#include")
  "Slang preprocessor keywords for tree-sitter font-locking.")

(defvar slang-ts-mode--keywords
  '("break" "case" "const" "continue"
    "default" "do" "else" "enum"
    "extern" "for" "goto" "if" "inline"
    "register" "restrict" "return"
    "sizeof" "static" "struct"
    "switch" "typedef" "union"
    "volatile" "while"
    "catch" "class" "concept" "consteval"
    "constexpr" "constinit" "decltype" "delete"
    "explicit" "final" "friend"
    "mutable" "namespace" "new" "noexcept"
    "operator" "override" "private" "protected"
    "public" "requires" "static_assert" "throw"
    "try" "typename" "using")
  "Slang keywords for tree-sitter font-locking.")

(defvar slang-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'slang
   :feature 'preprocessor
   `((preproc_directive) @font-lock-preprocessor-face
     (preproc_def
      name: (identifier) @font-lock-variable-name-face)
     (preproc_ifdef
      name: (identifier) @font-lock-variable-name-face)
     (preproc_function_def
      name: (identifier) @font-lock-function-name-face)
     (preproc_params
      (identifier) @font-lock-variable-name-face)
     (preproc_defined
      "defined" @font-lock-preprocessor-face
      "(" @font-lock-preprocessor-face
      (identifier) @font-lock-variable-name-face
      ")" @font-lock-preprocessor-face)
     [,@slang-ts-mode--preproc-keywords] @font-lock-preprocessor-face)

   :language 'slang
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'slang
   :feature 'attribute
   '((attribute_declaration "[[" @font-lock-bracket-face
                            "]]" @font-lock-bracket-face)
     (attribute_declaration (attribute name: (identifier) @font-lock-type-face))
     (attribute_declaration (attribute prefix: (identifier) @font-lock-type-face))
     (attribute_declaration (attribute "::" @font-lock-delimiter-face))
     (hlsl_attribute (_ (identifier) @font-lock-type-face))
     (hlsl_attribute (_  (_ "::" @font-lock-delimiter-face)))
     (hlsl_attribute "[" @font-lock-bracket-face
                     "]" @font-lock-bracket-face))

   :language 'slang
   :feature 'namespace
   '(((namespace_identifier) @font-lock-type-face)
     (qualified_identifier "::" @font-lock-delimiter-face)
     (qualified_identifier
      scope: (namespace_identifier) @font-lock-type-face
      name: (identifier) @font-lock-type-face))

   :language 'slang
   :feature 'type
   `((primitive_type) @font-lock-type-face
     (type_identifier) @font-lock-type-face)

   :language 'slang
   :feature 'number
   '((number_literal) @font-lock-number-face)

   :language 'slang
   :feature 'operator
   `([,@slang-ts-mode--operators] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :language 'slang
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'slang
   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)

   :language 'slang
   :feature 'string
   '((string_literal) @font-lock-string-face)

   :language 'slang
   :feature 'semantics
   '((bitfield_clause (identifier) @font-lock-preprocessor-face)
     (semantics (identifier) @font-lock-preprocessor-face))

   :language 'slang
   :feature 'keyword
   `([,@slang-ts-mode--keywords] @font-lock-keyword-face
     (this) @font-lock-keyword-face)

   :language 'slang
   :feature 'function
   '((function_declarator declarator: (identifier) @font-lock-function-name-face)
     (parameter_declaration (["in" "out" "inout"] @font-lock-keyword-face)
                            type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (call_expression
      function:
      [(template_function (identifier) @font-lock-function-call-face)
       (identifier) @font-lock-function-call-face
       (field_expression field: (field_identifier) @font-lock-function-call-face)]))

   :language 'slang
   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (null) @font-lock-constant-face)

   :language 'slang
   :feature 'property
   '((field_identifier) @font-lock-property-use-face)))

;;;###autoload
(define-derived-mode slang-ts-mode c-ts-base-mode "Slang"
  "Major mode for editing Slang, powered by tree-sitter.

Since this mode uses a parser, unbalanced brackets might cause
some breakage in indentation/fontification.  Therefore, it's
recommended to enable `electric-pair-mode' with this mode."
  :group 'slang

  (when (treesit-ensure-installed 'slang)
    (treesit-parser-create 'slang)

    ;; Indent.
    (slang-ts--apply-indent-rules c-ts-mode-indent-style)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings slang-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment preprocessor)
                  (keyword string semantics namespace)
                  (attribute constant number type)
                  (bracket delimiter function operator property)))

    (treesit-major-mode-setup)))

(derived-mode-add-parents 'slang-ts-mode '(slang-mode))

;;;###autoload
(defun slang-ts-mode-maybe ()
  "Enable `slang-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'slang)
          (eq treesit-enabled-modes t)
          (memq 'slang-ts-mode treesit-enabled-modes))
      (slang-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist
               '("\\(?:.slang\\|\\.sl\\|\\.slangh\\)\\'" . slang-ts-mode-maybe))
  ;; To be able to toggle between an external package and core ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(slang-mode . slang-ts-mode)))

(provide 'slang-ts-mode)

;;; slang-ts-mode.el ends here
