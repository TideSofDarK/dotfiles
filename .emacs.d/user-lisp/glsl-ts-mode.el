;;; glsl-ts-mode.el --- tree-sitter support for GLSL -*- lexical-binding: t -*-

;; Copyright (C) 2026 TideS

;; Author           : TideS <tidesmain@gmail.com>
;; Maintainer       : TideS <tidesmain@gmail.com>
;; Created          : June 2026
;; Keywords         : glsl languages tree-sitter
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

;; Major mode for editing OpenGL GLSL shader files using Tree-sitter.

;;; Code:

(require 'treesit)
(require 'c-ts-common)
(require 'c-ts-mode)

(add-to-list
 'treesit-language-source-alist
 '(glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
 t)

(defcustom glsl-ts-indent-offset 4
  "Number of spaces for each indentation step in `glsl-ts-mode'."
  :version "31.1"
  :type 'integer
  :safe 'integerp
  :group 'glsl)

(defvar glsl-ts--operators
  '("=" "-" "*" "/" "+" "%" "|" "&" "^" "<<" ">>" "." "<"
    "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-=" "+=" "*=" "/="
    "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++")
  "GLSL operators for tree-sitter font-locking.")

(defvar glsl-ts--keywords
  '("break" "continue" "do" "for" "while" "if" "else"
    "subroutine" "return" "switch" "default" "case")
  "GLSL keywords for tree-sitter font-locking.")

(defvar glsl-ts--constants
  '("gl_MaxVertexAttribs" "gl_MaxVertexUniformVectors"
    "gl_MaxVertexUniformComponents" "gl_MaxVertexOutputComponents"
    "gl_MaxVaryingComponents" "gl_MaxVaryingVectors"
    "gl_MaxVertexTextureImageUnits" "gl_MaxVertexImageUniforms"
    "gl_MaxVertexAtomicCounters" "gl_MaxVertexAtomicCounterBuffers"
    "gl_MaxTessPatchComponents" "gl_MaxPatchVertices" "gl_MaxTessGenLevel"
    "gl_MaxTessControlInputComponents" "gl_MaxTessControlOutputComponents"
    "gl_MaxTessControlTextureImageUnits" "gl_MaxTessControlUniformComponents"
    "gl_MaxTessControlTotalOutputComponents" "gl_MaxTessControlImageUniforms"
    "gl_MaxTessControlAtomicCounters" "gl_MaxTessControlAtomicCounterBuffers"
    "gl_MaxTessEvaluationInputComponents" "gl_MaxTessEvaluationOutputComponents"
    "gl_MaxTessEvaluationTextureImageUnits" "gl_MaxTessEvaluationUniformComponents"
    "gl_MaxTessEvaluationImageUniforms" "gl_MaxTessEvaluationAtomicCounters"
    "gl_MaxTessEvaluationAtomicCounterBuffers" "gl_MaxGeometryInputComponents"
    "gl_MaxGeometryOutputComponents" "gl_MaxGeometryImageUniforms"
    "gl_MaxGeometryTextureImageUnits" "gl_MaxGeometryOutputVertices"
    "gl_MaxGeometryTotalOutputComponents" "gl_MaxGeometryUniformComponents"
    "gl_MaxGeometryVaryingComponents" "gl_MaxGeometryAtomicCounters"
    "gl_MaxGeometryAtomicCounterBuffers" "gl_MaxFragmentImageUniforms"
    "gl_MaxFragmentInputComponents" "gl_MaxFragmentUniformVectors"
    "gl_MaxFragmentUniformComponents" "gl_MaxFragmentAtomicCounters"
    "gl_MaxFragmentAtomicCounterBuffers" "gl_MaxDrawBuffers"
    "gl_MaxTextureImageUnits" "gl_MinProgramTexelOffset" "gl_MaxProgramTexelOffset"
    "gl_MaxImageUnits" "gl_MaxSamples" "gl_MaxImageSamples" "gl_MaxClipDistances"
    "gl_MaxCullDistances" "gl_MaxViewports" "gl_MaxComputeImageUniforms"
    "gl_MaxComputeWorkGroupCount" "gl_MaxComputeWorkGroupSize"
    "gl_MaxComputeUniformComponents" "gl_MaxComputeTextureImageUnits"
    "gl_MaxComputeAtomicCounters" "gl_MaxComputeAtomicCounterBuffers"
    "gl_MaxCombinedTextureImageUnits" "gl_MaxCombinedImageUniforms"
    "gl_MaxCombinedImageUnitsAndFragmentOutputs"
    "gl_MaxCombinedShaderOutputResources" "gl_MaxCombinedAtomicCounters"
    "gl_MaxCombinedAtomicCounterBuffers" "gl_MaxCombinedClipAndCullDistances"
    "gl_MaxAtomicCounterBindings" "gl_MaxAtomicCounterBufferSize"
    "gl_MaxTransformFeedbackBuffers" "gl_MaxTransformFeedbackInterleavedComponents"
    "gl_MaxInputAttachments" "gl_RayFlagsNoneEXT" "gl_RayFlagsOpaqueEXT"
    "gl_RayFlagsNoOpaqueEXT" "gl_RayFlagsTerminateOnFirstHitEXT"
    "gl_RayFlagsSkipClosestHitShaderEXT" "gl_RayFlagsCullBackFacingTrianglesEXT"
    "gl_RayFlagsCullFrontFacingTrianglesEXT" "gl_RayFlagsCullOpaqueEXT"
    "gl_RayFlagsCullNoOpaqueEXT" "gl_RayFlagsForceOpacityMicromap2StateEXT"
    "gl_RayFlagsSkipTrianglesEXT" "gl_RayFlagsSkipAABBEXT"
    "gl_HitKindFrontFacingMicroTriangleNV" "gl_HitKindBackFacingMicroTriangleNV"
    "gl_HitKindFrontFacingTriangleEXT" "gl_HitKindBackFacingTriangleEXT")
  "GLSL constants for tree-sitter font-locking.")

(defvar glsl-ts--variables
  '("gl_VertexID" "gl_InstanceID" "gl_VertexIndex" "gl_InstanceIndex" "gl_DrawID"
    "gl_BaseVertex" "gl_BaseInstance" "gl_Position" "gl_PointSize"
    "gl_ClipDistance" "gl_CullDistance" "gl_FragCoord" "gl_FrontFacing"
    "gl_PointCoord" "gl_PrimitiveID" "gl_SampleID" "gl_SamplePosition"
    "gl_SampleMaskIn" "gl_Layer" "gl_ViewportIndex" "gl_HelperInvocation"
    "gl_FragDepth" "gl_SampleMask" "gl_in" "gl_PrimitiveIDIn" "gl_InvocationID"
    "gl_PatchVerticesIn" "gl_out" "gl_TessLevelOuter" "gl_TessLevelInner"
    "gl_PerVertex" "gl_TessCoord" "gl_NumWorkGroups" "gl_WorkGroupSize"
    "gl_WorkGroupID" "gl_LocalInvocationID" "gl_GlobalInvocationID"
    "gl_LocalInvocationIndex" "gl_PrimitivePointIndicesEXT"
    "gl_PrimitiveLineIndicesEXT" "gl_PrimitiveTriangleIndicesEXT"
    "gl_MeshPerPrimitiveEXT" "gl_MeshPrimitivesEXT" "gl_CullPrimitiveEXT"
    "gl_PrimitiveShadingRateEXT" "gl_LaunchIDEXT" "gl_LaunchSizeEXT"
    "gl_InstanceCustomIndexEXT" "gl_GeometryIndexEXT" "gl_WorldRayOriginEXT"
    "gl_WorldRayDirectionEXT" "gl_ObjectRayOriginEXT" "gl_ObjectRayDirectionEXT"
    "gl_RayTminEXT" "gl_RayTmaxEXT" "gl_IncomingRayFlagsEXT" "gl_ObjectToWorldEXT"
    "gl_ObjectToWorld3x4EXT" "gl_WorldToObjectEXT" "gl_WorldToObject3x4EXT"
    "gl_HitTEXT" "gl_HitKindEXT" "gl_HitMicroTriangleVertexPositionsNV"
    "gl_HitMicroTriangleVertexBarycentricsNV" "gl_CullMaskEXT"
    "gl_HitTriangleVertexPositionsEXT")
  "GLSL variables for tree-sitter font-locking.")

(defvar glsl-ts--builtins
  '("abs" "acos" "acosh" "all" "any" "anyInvocation" "allInvocations"
    "allInvocationsEqual" "asin" "asinh" "atan" "atanh" "atomicAdd" "atomicMin"
    "atomicMax" "atomicAnd" "atomicOr" "atomicXor" "atomicExchange"
    "atomicCompSwap" "atomicCounter" "atomicCounterDecrement"
    "atomicCounterIncrement" "atomicCounterAdd" "atomicCounterSubtract"
    "atomicCounterMin" "atomicCounterMax" "atomicCounterAnd" "atomicCounterOr"
    "atomicCounterXor" "atomicCounterExchange" "atomicCounterCompSwap" "barrier"
    "bitCount" "bitfieldExtract" "bitfieldInsert" "bitfieldReverse" "ceil" "clamp"
    "cos" "cosh" "cross" "degrees" "determinant" "dFdx" "dFdy" "dFdyFine"
    "dFdxFine" "dFdyCoarse" "dFdxCoarse" "distance" "dot" "fwidthFine"
    "fwidthCoarse" "EmitStreamVertex" "EmitStreamPrimitive" "EmitVertex"
    "EndPrimitive" "EndStreamPrimitive" "equal" "exp" "exp2" "faceforward"
    "findLSB" "findMSB" "floatBitsToInt" "floatBitsToUint" "floor" "fma" "fract"
    "frexp" "fwidth" "greaterThan" "greaterThanEqual" "groupMemoryBarrier"
    "imageAtomicAdd" "imageAtomicAnd" "imageAtomicCompSwap" "imageAtomicExchange"
    "imageAtomicMax" "imageAtomicMin" "imageAtomicOr" "imageAtomicXor" "imageLoad"
    "imageSize" "imageStore" "imulExtended" "intBitsToFloat" "imageSamples"
    "interpolateAtCentroid" "interpolateAtOffset" "interpolateAtSample" "inverse"
    "inversesqrt" "isinf" "isnan" "ldexp" "length" "lessThan" "lessThanEqual" "log"
    "log2" "matrixCompMult" "max" "memoryBarrier" "memoryBarrierAtomicCounter"
    "memoryBarrierBuffer" "memoryBarrierShared" "memoryBarrierImage" "min" "mix"
    "mod" "modf" "normalize" "not" "notEqual" "outerProduct" "packDouble2x32"
    "packHalf2x16" "packSnorm2x16" "packSnorm4x8" "packUnorm2x16" "packUnorm4x8"
    "pow" "radians" "reflect" "refract" "round" "roundEven" "sign" "sin" "sinh"
    "smoothstep" "sqrt" "step" "tan" "tanh" "texelFetch" "texelFetchOffset"
    "texture" "textureGather" "textureGatherOffset" "textureGatherOffsets"
    "textureGrad" "textureSamples" "textureGradOffset" "textureLod"
    "textureLodOffset" "textureOffset" "textureProj" "textureProjGrad"
    "textureProjGradOffset" "textureProjLod" "textureProjLodOffset"
    "textureProjOffset" "textureQueryLevels" "textureQueryLod" "textureSize"
    "transpose" "trunc" "uaddCarry" "uintBitsToFloat" "umulExtended"
    "unpackDouble2x32" "unpackHalf2x16" "unpackSnorm2x16" "unpackSnorm4x8"
    "unpackUnorm2x16" "unpackUnorm4x8" "usubBorrow" "nonuniformEXT" "traceRayEXT"
    "reportIntersectionEXT" "ignoreIntersectionEXT" "terminateRayEXT"
    "executeCallableEXT" "EmitMeshTasksEXT" "SetMeshOutputsEXT")
  "GLSL builtins for tree-sitter font-locking.")

(defvar glsl-ts--types
  '("float" "double" "int" "void" "bool" "mat2" "mat3"
    "mat4" "dmat2" "dmat3" "dmat4" "mat2x2" "mat2x3" "mat2x4" "dmat2x2"
    "dmat2x3" "dmat2x4" "mat3x2" "mat3x3" "mat3x4" "dmat3x2" "dmat3x3"
    "dmat3x4" "mat4x2" "mat4x3" "mat4x4" "dmat4x2" "dmat4x3" "dmat4x4" "vec2"
    "vec3" "vec4" "ivec2" "ivec3" "ivec4" "bvec2" "bvec3" "bvec4" "dvec2"
    "dvec3" "dvec4" "uint" "uvec2" "uvec3" "uvec4" "atomic_uint"
    "sampler1D" "sampler2D" "sampler3D" "samplerCube" "sampler1DShadow"
    "sampler2DShadow" "samplerCubeShadow" "sampler1DArray" "sampler2DArray"
    "sampler1DArrayShadow" "sampler2DArrayShadow" "isampler1D" "isampler2D"
    "isampler3D" "isamplerCube" "isampler1DArray" "isampler2DArray"
    "usampler1D" "usampler2D" "usampler3D" "usamplerCube" "usampler1DArray"
    "usampler2DArray" "sampler2DRect" "sampler2DRectShadow" "isampler2DRect"
    "usampler2DRect" "samplerBuffer" "isamplerBuffer" "usamplerBuffer"
    "sampler2DMS" "isampler2DMS" "usampler2DMS" "sampler2DMSArray"
    "isampler2DMSArray" "usampler2DMSArray" "samplerCubeArray"
    "samplerCubeArrayShadow" "isamplerCubeArray" "usamplerCubeArray"
    "image1D" "iimage1D" "uimage1D" "image2D" "iimage2D" "uimage2D" "image3D"
    "iimage3D" "uimage3D" "image2DRect" "iimage2DRect" "uimage2DRect"
    "imageCube" "iimageCube" "uimageCube" "imageBuffer" "iimageBuffer"
    "uimageBuffer" "image1DArray" "iimage1DArray" "uimage1DArray"
    "image2DArray" "iimage2DArray" "uimage2DArray" "imageCubeArray"
    "iimageCubeArray" "uimageCubeArray" "image2DMS" "iimage2DMS" "uimage2DMS"
    "image2DMSArray" "iimage2DMSArray" "uimage2DMSArray")
  "GLSL types for tree-sitter font-locking.")

(defvar glsl-ts--qualifiers
  '("attribute" "const" "uniform" "varying" "buffer" "shared" "coherent"
    "volatile" "restrict" "readonly" "writeonly" "layout" "centroid" "flat"
    "smooth" "noperspective" "patch" "sample" "in" "out" "inout"
    "invariant" "lowp" "mediump" "highp")
  "GLSL qualifiers for tree-sitter font-locking.")

(defvar glsl-ts--font-lock-settings
  (treesit-font-lock-rules
   :language 'glsl
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'glsl
   :feature 'preprocessor
   `((preproc_call (preproc_directive) @font-lock-preprocessor-face
                   ;; ((preproc_arg) @glsl-extension-face)
                   )
     (preproc_if (identifier) @font-lock-constant-face)
     (preproc_if condition: (_ (identifier) @font-lock-constant-face))
     (preproc_elif (identifier) @font-lock-constant-face)
     (preproc_elif condition: (_ (identifier) @font-lock-constant-face))
     (preproc_ifdef name: (identifier) @font-lock-constant-face)
     (preproc_def name: (identifier) @font-lock-constant-face)
     (preproc_function_def name: ((identifier) @font-lock-function-name-face))
     (preproc_include ((string_literal) @font-lock-string-face))
     (["#if", "#ifdef", "#ifndef", "#else", "#elif", "#endif", "#define", "#include"]
      @font-lock-preprocessor-face)
     (preproc_extension (preproc_directive) @font-lock-preprocessor-face
                        ;; extension: (identifier) @glsl-extension-face
                        ((extension_behavior) @font-lock-keyword-face
                         (:match "require\\|enable" @font-lock-keyword-face)))
     (preproc_extension (preproc_directive) @font-lock-preprocessor-face
                        ;; extension: (identifier) @glsl-extension-face
                        ((extension_behavior) @font-lock-warning-face
                         (:match "warn\\|disable" @font-lock-warning-face)))
     (preproc_params
      (identifier) @font-lock-variable-name-face)
     (preproc_defined
      "defined" @font-lock-preprocessor-face
      "(" @font-lock-preprocessor-face
      (identifier) @font-lock-variable-name-face
      ")" @font-lock-preprocessor-face))

   :language 'glsl
   :feature 'definition
   `((function_declarator declarator: (_) @font-lock-function-name-face)
     (struct_specifier "struct" @font-lock-keyword-face)
     (declaration "uniform" @font-lock-keyword-face)
     (declaration "shared" @font-lock-keyword-face)
     (declaration (layout_specification "layout" @font-lock-keyword-face)
                  [,@glsl-ts--qualifiers] @font-lock-keyword-face
                  (identifier) @font-lock-variable-name-face)
     (declaration (layout_specification "layout" @font-lock-keyword-face)
                  (extension_storage_class) @font-lock-keyword-face
                  (identifier) @font-lock-variable-name-face)
     (declaration (layout_specification "layout" @font-lock-keyword-face))
     (declaration (extension_storage_class ["hitAttributeEXT"] @font-lock-keyword-face))
     (declaration type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (init_declarator declarator: (identifier) @font-lock-variable-name-face)
     (parameter_declaration type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (parameter_declaration (["in" "out" "inout"] @font-lock-keyword-face)
                            type: (_) declarator: (identifier) @font-lock-variable-name-face)
     (array_declarator declarator: (identifier) @font-lock-variable-name-face)
     (call_expression function:
                      ((subscript_expression argument: (identifier) @font-lock-type-face)
                       (:match ,(rx-to-string `(seq bol (or ,@glsl-ts--types) eol)) @font-lock-type-face)))
     (call_expression function:
                      ((identifier) @font-lock-type-face
                       (:match ,(rx-to-string `(seq bol (or ,@glsl-ts--types) eol)) @font-lock-type-face))))

   :language 'glsl
   :feature 'keyword
   `((conditional_expression (["?" ":"]) @font-lock-keyword-face)
     (expression_statement (identifier) @font-lock-keyword-face
                           (:match "discard" @font-lock-keyword-face))
     [,@glsl-ts--keywords] @font-lock-keyword-face)

   :language 'glsl
   :feature 'builtin
   `(((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string `(seq bol (or ,@glsl-ts--builtins) eol))
              @font-lock-builtin-face)))

   :language 'glsl
   :feature 'function
   '((call_expression function: (identifier) @font-lock-function-call-face))

   :language 'glsl
   :feature 'qualifier
   '(((type_qualifier) @font-lock-keyword-face))

   :language 'glsl
   :feature 'operator
   `([,@glsl-ts--operators] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :language 'glsl
   :feature 'literal
   '((number_literal) @font-lock-number-face)

   :language 'glsl
   :feature 'type
   '(((primitive_type) @font-lock-type-face)
     ((type_identifier) @font-lock-type-face))

   :language 'glsl
   :feature 'constant
   `(((identifier) @font-lock-constant-face
      (:match ,(rx-to-string `(seq bol (or ,@glsl-ts--constants))) @font-lock-constant-face))
     ((identifier) @font-lock-constant-face
      (:match ,(rx-to-string `(seq bol (or ,@glsl-ts--variables))) @font-lock-constant-face)))

   :language 'glsl
   :feature 'property
   '((field_identifier) @font-lock-property-use-face)

   :language 'glsl
   :feature 'delimiter
   '([";" "," ":"] @font-lock-bracket-face)

   :language 'glsl
   :feature 'bracket
   '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face)))

(defvar glsl-ts-other-file-alist
  '(("\\.frag$" (".vert"))
    ("\\.vert$" (".frag"))
    ("\\.frag.glsl$" (".vert.glsl"))
    ("\\.vert.glsl$" (".frag.glsl")))
  "Alist of extensions to find given the current file's extension.")

;;;###autoload
(define-derived-mode glsl-ts-mode c-ts-base-mode "GLSL"
  "Major mode for editing GLSL, powered by tree-sitter.

Since this mode uses a parser, unbalanced brackets might cause
some breakage in indentation/fontification.  Therefore, it's
recommended to enable `electric-pair-mode' with this mode."
  :group 'glsl

  (when (and (treesit-ready-p 'glsl t)
             (treesit-ensure-installed 'glsl))
    (treesit-parser-create 'glsl)

    (setq-local ff-other-file-alist 'glsl-ts-other-file-alist)

    (setq-local c-ts-common-indent-offset 'glsl-ts-indent-offset)
    (setq-local treesit-simple-indent-rules
                (c-ts-mode--simple-indent-rules
                 'cpp c-ts-mode-indent-style))
    (setcar (car treesit-simple-indent-rules) 'glsl)

    (setq-local treesit-font-lock-settings glsl-ts--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment document definition)
                  (keyword preprocessor string type qualifier builtin)
                  (assignment constant escape-sequence literal)
                  (bracket delimiter error function operator property variable)))
    
    (treesit-major-mode-setup)))

;;;###autoload
(defun glsl-ts-mode-maybe ()
  "Enable `glsl-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'glsl)
          (eq treesit-enabled-modes t)
          (memq 'glsl-ts-mode treesit-enabled-modes))
      (glsl-ts-mode)
    (prog-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist
               '("\\(?:.glsl\\|\\.frag.glsl\\|\\.vert.glsl\\)\\'" . glsl-ts-mode-maybe))
  ;; To be able to toggle between an external package and this ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(glsl-mode . glsl-ts-mode)))

(provide 'glsl-ts-mode)

;;; glsl-ts-mode.el ends here
