;;; c-ts-mode-extras.el --- Extra rules for c{++}-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 TideS

;; Author: TideS <tidesmain@gmail.com>
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "31.1"))
;; Version: 1.0

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

;;; Code:

(require 'c-ts-mode)
(eval-when-compile
  (require 'cl-lib))

(defgroup c-ts-mode-extras nil
  "Extra faces for tree-sitter."
  :group 'faces)

(defface c-ts-mode-extras-boolean-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Custom face for tree-sitter. Used with `true', `false' keywords."
  :group 'c-ts-mode-extras)

(defface c-ts-mode-extras-this-face
  '((t :inherit font-lock-keyword-face))
  "Custom face for tree-sitter. Used with `this' keywords."
  :group 'c-ts-mode-extras)

(defface c-ts-mode-extras-enumerator-face
  '((t :inherit font-lock-constant-face))
  "Custom enumerator face for tree-sitter"
  :group 'c-ts-mode-extras)

(defface c-ts-mode-extras-field-face
  '((t :inherit font-lock-property-name-face))
  "Custom field face for tree-sitter."
  :group 'c-ts-mode-extras)

(defface c-ts-mode-extras-parameter-face
  '((t :inherit font-lock-variable-name-face))
  "Custom parameter face for tree-sitter."
  :group 'c-ts-mode-extras)

(defface c-ts-mode-extras-named-operator-face
  '((t :inherit font-lock-keyword-face))
  "Custom named operator face for tree-sitter."
  :group 'c-ts-mode-extras)

(defface c-ts-mode-extras-label-face
  '((t :inherit font-lock-keyword-face :weight semibold :slant normal))
  "Custom label face for C/C++ tree-sitter."
  :group 'c-ts-mode-extras)

(defvar c-ts-mode-extras-uppercase-is-const t
  "Highlight uppercase identifiers with `font-lock-constant-face'.")

(defvar c-ts-mode-extras--feature-list
  '(( attribute)
     ( )
     ( )
     ( parameter))
  "Extra font-lock features for `c-ts-mode'.")

(defvar c-ts-mode-extras--constant-regex "\\`[A-Z_][A-Z0-9_]+\\'" "")

(defun c-ts-mode-extras--non-const-field (node)
  "Return non-nil if NODE is not a constant field identifier."
  (not (when-let* ((parent (treesit-node-parent node)))
         (cl-dolist (child (treesit-node-children parent t))
           (when (string-match-p "const" (treesit-node-text child))
             (cl-return t))))))

(defun c-ts-mode-extras--rules (mode)
  (treesit-font-lock-rules
    :default-language mode

    :override t
    :feature 'parameter
    `(,@(when (eq mode 'cpp)
          '((parameter_declaration
              declarator: (_ (identifier) @c-ts-mode-extras-parameter-face))
             (template_parameter_list
               (parameter_declaration
                 declarator: (_) @c-ts-mode-extras-parameter-face))))
       (parameter_declaration
         declarator:
         (identifier) @c-ts-mode-extras-parameter-face)
       (parameter_declaration
         declarator:
         (pointer_declarator
           declarator:
           (identifier) @c-ts-mode-extras-parameter-face))
       (parameter_declaration
         declarator:
         (pointer_declarator
           declarator:
           (pointer_declarator
             declarator:
             (identifier) @c-ts-mode-extras-parameter-face)))
       (parameter_declaration
         declarator:
         (pointer_declarator
           declarator:
           (pointer_declarator
             declarator:
             (pointer_declarator
               declarator:
               (identifier) @c-ts-mode-extras-parameter-face))))
       (parameter_declaration
         declarator:
         (function_declarator
           declarator:
           (parenthesized_declarator
             (_ declarator: (identifier) @c-ts-mode-extras-parameter-face)))))

    :override t
    :feature 'delimiter
    `(,@(when (eq mode 'cpp)
          '(("::" @font-lock-punctuation-face)))
       ("." @font-lock-punctuation-face))

    :override t
    :feature 'bracket
    `(,@(when (eq mode 'cpp)
          '((template_parameter_list (["<" ">"]) @font-lock-punctuation-face))))

    :override t
    :feature 'function
    `(,@(when (eq mode 'c)
          '((call_expression
              function: (identifier) @font-lock-function-call-face)
             (call_expression
               function: (field_expression
                           field: (field_identifier) @font-lock-function-call-face))))
       ,@(when (eq mode 'cpp)
           `((function_declarator
               declarator: ([(field_identifier) (identifier)])
               @font-lock-function-name-face)
              (field_declaration
                type: (placeholder_type_specifier (auto))
                declarator: (field_identifier)
                @font-lock-function-name-face)
              (declaration
                declarator:
                (function_declarator
                  declarator: (identifier) @font-lock-function-name-face))
              (template_function
                name: (identifier) @font-lock-function-name-face)
              (call_expression
                function:
                (qualified_identifier
                  name:
                  (qualified_identifier
                    name: (identifier) @font-lock-function-call-face)))
              (call_expression
                function: (qualified_identifier
                            scope: (namespace_identifier)
                            name: (identifier) @font-lock-function-call-face))
              (call_expression
                function: (template_function
                            name: (identifier) @font-lock-function-call-face)))))

    :override t
    :feature 'label
    '((labeled_statement label: (_) @c-ts-mode-extras-label-face)
       (goto_statement label: (_) @c-ts-mode-extras-label-face))

    :override t
    :feature 'attribute
    '((attribute_declaration) @font-lock-constant-face)

    :override t
    :feature 'keyword
    `(,@(when (eq mode 'c)
          '(["_Alignas" "_Alignof"] @font-lock-keyword-face))
       ,@(when (eq mode 'cpp)
           '(((this) @c-ts-mode-extras-this-face)
              (function_definition
                type:
                (type_identifier) @font-lock-keyword-face
                (:match "\\`compl\\'" @font-lock-keyword-face))))
       (["alignas" "alignof"] @font-lock-keyword-face)
       ((null) @font-lock-keyword-face (:match "nullptr" @font-lock-keyword-face)))

    :override t
    :feature 'constant
    `(,@(when c-ts-mode-extras-uppercase-is-const
          `(((field_identifier) @font-lock-constant-face
              (:match ,c-ts-mode-extras--constant-regex @font-lock-constant-face))
             ((identifier) @font-lock-constant-face
               (:match ,c-ts-mode-extras--constant-regex @font-lock-constant-face))))
       [(true) (false)] @c-ts-mode-extras-boolean-face
       (case_statement value: (identifier) @font-lock-constant-face)
       (enumerator name: (identifier) @c-ts-mode-extras-enumerator-face))

    :override t
    :feature 'literal
    '((char_literal _ @font-lock-string-face)
       (char_literal (escape_sequence) @font-lock-escape-face))

    :override t
    :feature 'property
    `(,@(when (eq mode 'c)
          '((field_identifier) @c-ts-mode-extras-field-face))
       (field_declaration
         declarator: (field_identifier) @c-ts-mode-extras-field-face
         (:pred
           c-ts-mode-extras--non-const-field
           @c-ts-mode-extras-field-face))
       (pointer_declarator
         declarator: (field_identifier) @c-ts-mode-extras-field-face
         (:pred
           c-ts-mode-extras--non-const-field
           @c-ts-mode-extras-field-face))
       (initializer_pair
         designator: (field_designator
                       (field_identifier) @c-ts-mode-extras-field-face))
       (field_expression
         field: (field_identifier) @c-ts-mode-extras-field-face))

    :override t
    :feature 'operator
    `(,@(when (eq mode 'cpp)
          '((new_expression "new" @c-ts-mode-extras-named-operator-face)
             (delete_expression "delete" @c-ts-mode-extras-named-operator-face)
             (operator_name "[]" @font-lock-operator-face)
             (operator_name "()" @font-lock-operator-face)))
       (unary_expression "!" @font-lock-negation-char-face)
       (conditional_expression (["?" ":"]) @font-lock-operator-face))

    :override t
    :feature 'type
    `(,@(when (eq mode 'cpp)
          '((template_argument_list
              (type_descriptor
                type: (type_identifier) @font-lock-type-face))
             (template_type name: (type_identifier) @font-lock-type-face)
             (concept_definition name: (_) @font-lock-type-face)
             (using_declaration (identifier) @font-lock-type-face)
             (using_declaration
               (qualified_identifier
                 scope: (namespace_identifier)
                 name: (identifier) @font-lock-type-face))
             (namespace_identifier) @font-lock-type-face
             (call_expression
               function:
               (qualified_identifier
                 scope: (namespace_identifier)
                 name: (identifier) @font-lock-function-call-face))
             (function_declarator
               declarator:
               (qualified_identifier
                 scope: (namespace_identifier)
                 name: (identifier) @font-lock-function-name-face))))
       (sized_type_specifier) @font-lock-builtin-face
       (primitive_type) @font-lock-builtin-face)

    :override t
    :feature 'preprocessor
    `(,@(when (eq mode 'c)
          '((macro_type_specifier name: (identifier) @font-lock-preprocessor-face)))
       (preproc_call directive: (_) @font-lock-preprocessor-face)
       (preproc_defined
         "defined" @font-lock-function-call-face
         "(" @font-lock-punctuation-face
         (identifier) @font-lock-preprocessor-face
         ")" @font-lock-punctuation-face)
       (preproc_def name: (_) @font-lock-preprocessor-face)
       (preproc_function_def name: (_) @font-lock-preprocessor-face)
       (preproc_ifdef name: (_) @font-lock-preprocessor-face)
       (preproc_params
         "(" @font-lock-punctuation-face
         (identifier) @c-ts-mode-extras-parameter-face
         ")" @font-lock-punctuation-face)
       ((null) @font-lock-preprocessor-face
         (:match "NULL" @font-lock-preprocessor-face)))))

(add-hook 'c++-ts-mode-hook
  (lambda ()
    (setq-local treesit-font-lock-feature-list
      (treesit-merge-font-lock-feature-list
        treesit-font-lock-feature-list
        c-ts-mode-extras--feature-list))
    (treesit-add-font-lock-rules (c-ts-mode-extras--rules 'cpp))))
(add-hook 'c-ts-mode-hook
  (lambda ()
    (setq-local treesit-font-lock-feature-list
      (treesit-merge-font-lock-feature-list
        treesit-font-lock-feature-list
        c-ts-mode-extras--feature-list))
    (treesit-add-font-lock-rules (c-ts-mode-extras--rules 'c))))

(provide 'c-ts-mode-extras)

;;; c-ts-mode-extras.el ends here
