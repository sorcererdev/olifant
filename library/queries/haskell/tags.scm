; Olifant's own (not vendored): tree-sitter-haskell 0.23.1 ships no tags.scm.
; A function is its signature and its equations: each is captured, and the
; rule takes adjacent definitions of one name as one.
(signature name: (variable) @name) @definition.function
(function name: (variable) @name) @definition.function
(bind name: (variable) @name) @definition.function
(data_type name: (name) @name) @definition.type
(newtype name: (name) @name) @definition.type
(class name: (name) @name) @definition.class
(instance name: (name) @name) @definition.class
