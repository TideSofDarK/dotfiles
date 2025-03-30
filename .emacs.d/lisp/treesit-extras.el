;;; treesit-extras.el --- Extra highlighting and indentation rules for *-ts-modes -*- lexical-binding: t; -*-

(defgroup treesit-extras nil
  "Extra faces for tree-sitter."
  :group 'faces)

(defface treesit-extras-boolean-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Custom face for tree-sitter. Sometimes used with 'true', 'false' keywords."
  :group 'font-lock-faces)

(defface treesit-extras-null-face
  '((t :inherit font-lock-builtin-face))
  "Custom face for tree-sitter. Sometimes used with 'NULL' or 'nullptr'."
  :group 'font-lock-faces)

(defface treesit-extras-special-keyword-1-face
  '((t :inherit font-lock-keyword-face))
  "Custom face for tree-sitter. Sometimes used with 'this' keywords."
  :group 'font-lock-faces)

(defface treesit-extras-special-keyword-2-face
  '((t :inherit font-lock-keyword-face))
  "Custom face for tree-sitter. Sometimes used with 'return' keywords."
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
  '((t :inherit font-lock-operator-face :weight bold))
  "Custom 'new' face for C/C++ tree-sitter."
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
  :config
  (setq c-ts-mode-indent-offset 4)
  (setq c-ts-mode-indent-style #'treesit-extras--c-ts-indent-style)
  (setq c-ts-mode-enable-doxygen t)
  (setq c-ts-mode--preproc-keywords '("#include"))
  (advice-add 'c-ts-mode--keywords :around #'treesit-extras--c-ts-keywords)
  (defvar treesit-extras--c-ts-mode-constants
    `(
       ((field_identifier) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))
       ((identifier) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))))
  (defvar treesit-extras--c-ts-mode-common
    `(
       (parameter_declaration declarator: (identifier) @treesit-extras-parameter-face)
       (parameter_declaration declarator: (pointer_declarator declarator: (identifier) @treesit-extras-parameter-face))
       (parameter_declaration declarator: (pointer_declarator declarator: (pointer_declarator declarator: (identifier) @treesit-extras-parameter-face)))
       (parameter_declaration declarator: (pointer_declarator declarator: (pointer_declarator declarator: (pointer_declarator declarator: (identifier) @treesit-extras-parameter-face))))
       (parameter_declaration declarator: (function_declarator declarator: (parenthesized_declarator (_ declarator: (identifier) @treesit-extras-parameter-face))))
       (field_identifier) @treesit-extras-field-face
       (conditional_expression (["?" ":"]) @font-lock-operator-face)
       [(true) (false)] @treesit-extras-boolean-face
       (null) @treesit-extras-null-face
       (char_literal "'" @font-lock-string-face)
       (escape_sequence) @treesit-extras-named-operator-face
       (case_statement value: (identifier) @font-lock-constant-face)
       (sizeof_expression "sizeof" @treesit-extras-named-operator-face)
       (labeled_statement label: (_) @treesit-extras-label-face)
       (goto_statement label: (_) @treesit-extras-label-face)
       ("return" @treesit-extras-special-keyword-2-face)
       (sized_type_specifier) @font-lock-builtin-face
       (primitive_type) @font-lock-builtin-face
       (enumerator
         name: (identifier) @treesit-extras-enumerator-face)))
  (defvar treesit-extras--c-ts-mode-preprocessor
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
  (add-hook 'c-ts-mode-hook
    (lambda()
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'c
               :override t
               :feature 'constants
               treesit-extras--c-ts-mode-constants)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'c
               :override t
               :feature 'common
               treesit-extras--c-ts-mode-common)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'c
               :override t
               :feature 'preprocessor
               treesit-extras--c-ts-mode-preprocessor)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'c
               :override t
               :feature 'c-only-overrides
               `((macro_type_specifier name: (identifier) @font-lock-function-call-face)))) t)))
  (add-hook 'c++-ts-mode-hook
    (lambda()
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'common
               treesit-extras--c-ts-mode-common)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'fields
               `(
                  (function_declarator declarator: ([(field_identifier) (identifier)]) @font-lock-function-name-face)
                  (field_declaration type: (placeholder_type_specifier (auto)) declarator: (field_identifier) @font-lock-function-name-face)))) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'constants
               treesit-extras--c-ts-mode-constants)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'preprocessor
               treesit-extras--c-ts-mode-preprocessor)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'namespace-types
               `(
                  (using_declaration (identifier) @font-lock-type-face)
                  (using_declaration (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-type-face))
                  (namespace_identifier) @font-lock-type-face
                  (qualified_identifier scope: (namespace_identifier) name: (qualified_identifier scope: (namespace_identifier) name: (identifier)  @font-lock-type-face))))) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'namespace-functions
               `(
                  (call_expression function: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-call-face))
                  (function_declarator declarator: (qualified_identifier scope: (namespace_identifier) name: (identifier) @font-lock-function-name-face))))) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'cpp
               :override t
               :feature 'cpp-overrides
               '(
                  (declaration declarator: (function_declarator declarator: (identifier) @font-lock-function-name-face))
                  (parameter_declaration declarator: (_ (identifier) @treesit-extras-parameter-face))
                  ((this) @treesit-extras-special-keyword-1-face)
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
                  (call_expression
                    function:
                    (qualified_identifier scope: (namespace_identifier)
                      name: (identifier) @font-lock-function-call-face))
                  (call_expression
                    function:
                    (template_function name: (identifier) @font-lock-function-call-face))
                  )))) t)))

(use-package gdscript-ts-mode
  :ensure nil
  :after gdscript-mode
  :config
  (defvar treesit-extras--gdscript-ts-mode-punctuation '("[" "]" "(" ")" "{" "}" "," ":"))
  (defvar treesit-extras--gdscript-ts-mode-operators
    '("%" "%=" "->" "." "!=" "+=" "-="
       "/=" "*=" "==" ">>" "<<" "~"
       "&" "|" "&=" "|=" "-" ">="
       "<=" "||" "&&" ">>=" "<<="
       "^="))
  (defvar treesit-extras--gdscript-ts-mode-named-operators
    '("not" "in" "and" "is"))
  (defvar treesit-extras--gdscript-ts-mode-types
    `((identifier) @font-lock-type-face (:match "\\`[A-Z][a-zA-Z0-9_]*[a-z][a-zA-Z0-9_]*\\'" @font-lock-type-face)))
  (defvar treesit-extras--gdscript-ts-mode-builtin-classes-regex
    (rx (| "Camera2D" "Camera3D" "Control" "Node2D" "Node3D" "Vector2" "Vector2i" "Vector3" "Vector3i" "Vector4" "Vector4i" "Color" "Rect2" "Rect2i" "Array" "Basis" "Dictionary" "Plane" "Quat" "RID" "Rect3" "Transform" "Transform2D" "Transform3D" "AABB" "String" "NodePath" "PoolByteArray" "PoolIntArray" "PoolRealArray" "PoolStringArray" "PoolVector2Array" "PoolVector3Array" "PoolColorArray" "Signal" "Callable" "StringName" "Quaternion" "Projection" "PackedByteArray" "PackedInt32Array" "PackedInt64Array" "PackedFloat32Array" "PackedFloat64Array" "PackedStringArray" "PackedVector2Array" "PackedVector2iArray" "PackedVector3Array" "PackedVector3iArray" "PackedVector4Array" "PackedColorArray" "JSON" "UPNP" "OS" "IP" "JSONRPC" "XRVRS")))
  (defvar treesit-extras--gdscript-ts-mode-constants
    `(
       (const_statement name: (name) @font-lock-constant-face)
       ((identifier) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))
       (variable_statement name: (name) @font-lock-constant-face (:match ,treesit-extras--constant-regex @font-lock-constant-face))))
  (defvar treesit-extras--gdscript-ts-mode-overrides
    `(
       ;; ((identifier) @font-lock-builtin-face (:match ,treesit-extras--gdscript-ts-mode-builtin-classes-regex @font-lock-builtin-face))
       (escape_sequence) @treesit-extras-named-operator-face
       ((identifier) @font-lock-type-face (:match ,treesit-extras--gdscript-ts-mode-builtin-classes-regex @font-lock-type-face))
       (signal_statement (name) @font-lock-function-call-face)
       [(true) (false)] @treesit-extras-boolean-face
       (null) @treesit-extras-null-face
       (attribute (identifier) @treesit-extras-special-keyword-1-face (:match ,(rx (| "self")) @treesit-extras-special-keyword-1-face))
       (return_statement "return" @treesit-extras-special-keyword-2-face)
       ([,@treesit-extras--gdscript-ts-mode-punctuation] @font-lock-punctuation-face)
       ([,@treesit-extras--gdscript-ts-mode-operators] @font-lock-operator-face)
       ([,@treesit-extras--gdscript-ts-mode-named-operators] @treesit-extras-named-operator-face)
       (enum_definition name: (_) @font-lock-type-face)
       (enumerator left: (identifier) @treesit-extras-enumerator-face)
       (annotation "@" @font-lock-preprocessor-face (identifier) @font-lock-preprocessor-face)))
  (add-hook 'gdscript-ts-mode-hook
    (lambda()
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'gdscript
               :override t
               :feature 'better-types
               `(,treesit-extras--gdscript-ts-mode-types))) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'gdscript
               :override t
               :feature 'better-constants
               treesit-extras--gdscript-ts-mode-constants)) t)
      (add-to-list 'treesit-font-lock-settings
        (car (treesit-font-lock-rules
               :language 'gdscript
               :override t
               :feature 'overrides
               treesit-extras--gdscript-ts-mode-overrides)) t))))

(provide 'treesit-extras)
