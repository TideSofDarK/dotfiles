;;; treesit-extras.el --- Extra highlighting and indentation rules for *-ts-modes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 TideS

;; Author: TideS <tidesmain@gmail.com>

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

(defgroup treesit-extras nil
  "Extra faces for tree-sitter."
  :group 'faces)

(defface treesit-extras-boolean-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Custom face for tree-sitter. Used with 'true', 'false' keywords."
  :group 'font-lock-faces)

(defface treesit-extras-null-face
  '((t :inherit font-lock-builtin-face))
  "Custom face for tree-sitter. Used with 'NULL' or 'nullptr'."
  :group 'font-lock-faces)

(defface treesit-extras-this-face
  '((t :inherit font-lock-keyword-face))
  "Custom face for tree-sitter. Used with 'this' keywords."
  :group 'font-lock-faces)

(defface treesit-extras-return-face
  '((t :inherit font-lock-keyword-face))
  "Custom face for tree-sitter. Used with 'return' keywords."
  :group 'font-lock-faces)

(defface treesit-extras-enumerator-face
  '((t :inherit font-lock-constant-face))
  "Custom enumerator face for tree-sitter"
  :group 'font-lock-faces)

(defface treesit-extras-field-face
  '((t :inherit font-lock-property-name-face))
  "Custom field face for tree-sitter."
  :group 'font-lock-faces)

(defface treesit-extras-parameter-face
  '((t :inherit font-lock-variable-name-face))
  "Custom parameter face for tree-sitter."
  :group 'font-lock-faces)

(defface treesit-extras-named-operator-face
  '((t :inherit font-lock-keyword-face))
  "Custom named operator face for tree-sitter."
  :group 'font-lock-faces)

(defface treesit-extras-label-face
  '((t :inherit font-lock-keyword-face :weight semibold :slant normal))
  "Custom label face for C/C++ tree-sitter."
  :group 'font-lock-faces)

(defvar treesit-extras--constant-regex "\\`[A-Z_][A-Z0-9_]+\\'" "")

(use-package c-ts-mode
  :ensure nil
  :preface
  (defun treesit-extras--c-ts-indent-style()
    `(
       ((node-is "preproc") column-0 0)
       ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
       ((parent-is "argument_list") prev-sibling 0)
       ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
       ((parent-is "parameter_list") prev-sibling 0)
       ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
  (defun treesit-extras--c-ts-keywords (orig-fun &rest args)
    `("#if" "#ifdef" "#ifndef" "#elif" "#else" "#endif" "#define",@(apply orig-fun args)))
  (defconst treesit-extras--c-ts-mode-constants
    `(
       ((field_identifier) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))
       ((identifier) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))))
  (defconst treesit-extras--c-ts-mode-common
    `(("." @font-lock-punctuation-face)
       (attribute_declaration) @font-lock-constant-face
       (parameter_declaration declarator: (identifier) @treesit-extras-parameter-face)
       (parameter_declaration declarator: (pointer_declarator declarator: (identifier) @treesit-extras-parameter-face))
       (parameter_declaration declarator: (pointer_declarator declarator: (pointer_declarator declarator: (identifier) @treesit-extras-parameter-face)))
       (parameter_declaration declarator: (pointer_declarator declarator: (pointer_declarator declarator: (pointer_declarator declarator: (identifier) @treesit-extras-parameter-face))))
       (parameter_declaration declarator: (function_declarator declarator: (parenthesized_declarator (_ declarator: (identifier) @treesit-extras-parameter-face))))
       (field_identifier) @treesit-extras-field-face
       (conditional_expression (["?" ":"]) @font-lock-keyword-face)
       [(true) (false)] @treesit-extras-boolean-face
       (null) @treesit-extras-null-face
       (char_literal "'" @font-lock-string-face)
       (char_literal (character) @font-lock-string-face)
       (case_statement value: (identifier) @font-lock-constant-face)
       (escape_sequence) @treesit-extras-named-operator-face
       (sizeof_expression "sizeof" @treesit-extras-named-operator-face)
       (labeled_statement label: (_) @treesit-extras-label-face)
       (goto_statement label: (_) @treesit-extras-label-face)
       ("return" @treesit-extras-return-face)
       (sized_type_specifier) @font-lock-builtin-face
       (primitive_type) @font-lock-builtin-face
       (enumerator
         name: (identifier) @treesit-extras-enumerator-face)))
  (defconst treesit-extras--c-ts-mode-preprocessor
    `(
       (call_expression function: (identifier) @font-lock-function-call-face)
       (call_expression function: (field_expression field: (field_identifier) @font-lock-function-call-face))
       (preproc_call directive: (_) @font-lock-keyword-face)
       (preproc_defined
         "defined" @font-lock-function-call-face
         "(" @font-lock-punctuation-face
         (identifier) @font-lock-constant-face
         ")" @font-lock-punctuation-face)
       (preproc_def name: (_) @font-lock-constant-face)
       (preproc_function_def name: (_) @font-lock-function-name-face)
       (preproc_ifdef name: (_) @font-lock-constant-face)
       (preproc_params
         "(" @font-lock-punctuation-face
         (identifier) @treesit-extras-parameter-face
         ")" @font-lock-punctuation-face)))
  :config
  (setq c-ts-mode-indent-offset 4)
  (setq c-ts-mode-indent-style #'treesit-extras--c-ts-indent-style)
  (setq c-ts-mode-enable-doxygen t)
  (setq c-ts-mode--preproc-keywords '("#include"))
  (advice-add 'c-ts-mode--keywords :around #'treesit-extras--c-ts-keywords)
  (push 'extras (nth 3 c-ts-mode--feature-list))
  (push 'extras-common (nth 3 c-ts-mode--feature-list))
  (push 'extras-preprocessor (nth 3 c-ts-mode--feature-list))
  (push 'extras-namespace-types (nth 3 c-ts-mode--feature-list))
  (push 'extras-namespace-functions (nth 3 c-ts-mode--feature-list))
  (push 'extras-fields (nth 3 c-ts-mode--feature-list))
  (push 'extras-constants (nth 3 c-ts-mode--feature-list))
  (defun treesit-extras--c-ts-mode-fontlock-settings (mode)
    (let ((res '()))
      (add-to-list 'res
        (car (treesit-font-lock-rules
               :language mode
               :override t
               :feature 'extras-common
               treesit-extras--c-ts-mode-common)) t)
      (add-to-list 'res
        (car (treesit-font-lock-rules
               :language mode
               :override t
               :feature 'extras-constants
               treesit-extras--c-ts-mode-constants)) t)
      (add-to-list 'res
        (car (treesit-font-lock-rules
               :language mode
               :override t
               :feature 'extras-fields
               `((field_expression field: (field_identifier) @treesit-extras-field-face)))) t)
      (if (eq mode 'c)
        (progn
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'c
                   :override t
                   :feature 'extras-preprocessor
                   treesit-extras--c-ts-mode-preprocessor)) t)
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'c
                   :override t
                   :feature 'extras
                   `((macro_type_specifier name: (identifier) @font-lock-function-call-face)))) t))
        (progn
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'cpp
                   :override t
                   :feature 'extras-fields
                   `((function_declarator declarator: ([(field_identifier) (identifier)]) @font-lock-function-name-face)
                      (field_declaration type: (placeholder_type_specifier (auto)) declarator: (field_identifier) @font-lock-function-name-face)))) t)
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'cpp
                   :override t
                   :feature 'extras-preprocessor
                   treesit-extras--c-ts-mode-preprocessor)) t)
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'cpp
                   :override t
                   :feature 'extras-namespace-types
                   `((using_declaration (identifier) @font-lock-type-face)
                      (using_declaration (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-type-face))
                      (namespace_identifier) @font-lock-type-face
                      (qualified_identifier scope: (namespace_identifier) name: (qualified_identifier scope: (namespace_identifier) name: (identifier)  @font-lock-type-face))))) t)
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'cpp
                   :override t
                   :feature 'extras-namespace-functions
                   `((call_expression function: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-call-face))
                      (function_declarator declarator: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-name-face))))) t)
          (add-to-list 'res
            (car (treesit-font-lock-rules
                   :language 'cpp
                   :override t
                   :feature 'extras
                   '((declaration declarator: (function_declarator declarator: (identifier) @font-lock-function-name-face))
                      (parameter_declaration declarator: (_ (identifier) @treesit-extras-parameter-face))
                      ((this) @treesit-extras-this-face)
                      (concept_definition name: (_) @font-lock-type-face)
                      (template_function name: (identifier) @font-lock-function-name-face)
                      (new_expression "new" @treesit-extras-named-operator-face)
                      (delete_expression "delete" @treesit-extras-named-operator-face)
                      (function_definition type: (type_identifier) @font-lock-keyword-face (:match "\\`compl\\'" @font-lock-keyword-face))
                      (template_parameter_list (["<" ">"]) @font-lock-punctuation-face)
                      (template_parameter_list (parameter_declaration declarator: (_) @treesit-extras-parameter-face))
                      (template_argument_list (["<" ">"]) @font-lock-punctuation-face)
                      (template_argument_list (type_descriptor type: (type_identifier) @font-lock-type-face))
                      (template_type name: (type_identifier) @font-lock-type-face)
                      ("::" @font-lock-punctuation-face)
                      (call_expression function: (qualified_identifier name: (qualified_identifier name: (identifier) @font-lock-function-call-face)))
                      (call_expression function: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-call-face))
                      (call_expression function: (template_function name: (identifier) @font-lock-function-call-face))))) t)))
      res))
  (defconst treesit-extras--c-ts-mode-fontlock-settings-c (treesit-extras--c-ts-mode-fontlock-settings 'c))
  (defconst treesit-extras--c-ts-mode-fontlock-settings-cpp (treesit-extras--c-ts-mode-fontlock-settings 'cpp))
  (defun treesit-extras--c-ts-mode-fontlock-settings-wrapper (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (if (eq (car args) 'c)
        (append res treesit-extras--c-ts-mode-fontlock-settings-c)
        (append res treesit-extras--c-ts-mode-fontlock-settings-cpp))))
  (advice-add 'c-ts-mode--font-lock-settings :around #'treesit-extras--c-ts-mode-fontlock-settings-wrapper))

;; (use-package gdscript-ts-mode
;;   :ensure nil
;;   :after gdscript-mode
;;   :preface
;;   (defvar treesit-extras--gdscript-ts-mode-punctuation '("[" "]" "(" ")" "{" "}" "," ":" "."))
;;   (defvar treesit-extras--gdscript-ts-mode-named-operators
;;     '("not" "in" "and" "is"))
;;   (defvar treesit-extras--gdscript-ts-mode-constants
;;     `(
;;        (const_statement name: (name) @font-lock-constant-face)
;;        ((identifier) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))
;;        (variable_statement name: (name) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))))
;;   (defvar treesit-extras--gdscript-ts-mode-overrides
;;     `(
;;        (await_expression "await" @font-lock-keyword-face)
;;        (static_keyword) @font-lock-keyword-face
;;        (escape_sequence) @treesit-extras-named-operator-face
;;        (signal_statement (name) @font-lock-function-call-face)
;;        [(true) (false)] @treesit-extras-boolean-face
;;        (null) @treesit-extras-null-face
;;        (attribute (identifier) @treesit-extras-this-face (:match ,(rx (| "self")) @treesit-extras-this-face))
;;        (return_statement "return" @treesit-extras-return-face)
;;        ([,@treesit-extras--gdscript-ts-mode-punctuation] @font-lock-punctuation-face)
;;        ([,@treesit-extras--gdscript-ts-mode-named-operators] @treesit-extras-named-operator-face)
;;        (enum_definition name: (_) @font-lock-type-face)
;;        (enumerator left: (identifier) @treesit-extras-enumerator-face)
;;        (annotation "@" @font-lock-preprocessor-face (identifier) @font-lock-preprocessor-face)))
;;   (defvar treesit-extras--gdscript-ts-mode-keywords '("and" "as" "break" "class" "class_name"
;;                                                            "const" "continue" "elif" "else" "enum" "export" "extends" "for" "func" "if" "in" "is"
;;                                                            "master" "match" "not" "onready" "or" "pass"  "puppet" "remote" "remotesync" "return" "setget" "signal"
;;                                                            "var" "while"))
;;   (defvar treesit-extras--gdscript-ts-mode-type-regex "\\`\\(int\\|bool\\|float\\|void\\|[A-Z][a-zA-Z0-9_]*[a-z][a-zA-Z0-9_]*\\)\\'")
;;   (defvar gdscript-ts--treesit-settings
;;     (treesit-font-lock-rules
;;       :language 'gdscript
;;       :feature 'comment
;;       `((comment) @font-lock-comment-face)

;;       :language 'gdscript
;;       :feature 'definition
;;       `(,@treesit-extras--gdscript-ts-mode-overrides
;;          (function_definition (name) @font-lock-function-name-face)
;;          (class_definition
;;            (name) @font-lock-function-name-face))

;;       :language 'gdscript
;;       :feature 'keyword
;;       `(([,@treesit-extras--gdscript-ts-mode-keywords] @font-lock-keyword-face)
;;          ([(false) (true)] @font-lock-keyword-face))

;;       :language 'gdscript
;;       :feature 'string
;;       '((string) @font-lock-string-face)

;;       :language 'gdscript
;;       :feature 'type
;;       `(((identifier) @font-lock-type-face (:match ,treesit-extras--gdscript-ts-mode-type-regex @font-lock-type-face))
;;          ((type) @font-lock-type-face)
;;          (get_node) @font-lock-type-face)

;;       :feature 'function
;;       :language 'gdscript
;;       '((call (identifier) @font-lock-function-call-face)
;;          (attribute_call (identifier) @font-lock-function-call-face))

;;       :language 'gdscript
;;       :feature 'variable
;;       `(,@treesit-extras--gdscript-ts-mode-constants)

;;       :feature 'number
;;       :language 'gdscript
;;       '(([(integer) (float)] @font-lock-number-face))

;;       :feature 'property
;;       :language 'gdscript
;;       '((attribute (identifier) (identifier) @font-lock-property-use-face))

;;       :feature 'operator
;;       :language 'gdscript
;;       `(["+" "-" "*" "/" "^" ">" "<" "=" "%" "%=" "->" "." "!=" "+="
;;           "-=" "/=" "*=" "==" ">>" "<<" "~" "&" "|" "&=" "|=" "-"
;;           ">=" "<=" "||" "&&" ">>=" "<<=" "^="] @font-lock-operator-face))))

(provide 'treesit-extras)

;;; treesit-extras.el ends here
