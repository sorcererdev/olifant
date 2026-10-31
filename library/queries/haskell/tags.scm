; Olifant's own (not vendored): tree-sitter-haskell 0.23.1 ships no tags.scm.
; A function is its signature and its equations: each is captured, and the
; rule takes adjacent definitions of one name as one.
(signature name: (variable) @name) @definition.function
(function name: (variable) @name) @definition.function
(bind name: (variable) @name) @definition.function
(data_type name: (name) @name) @definition.type
(newtype name: (name) @name) @definition.type
(class name: (name) @name) @definition.class
; an instance: named by its class for a phrase (`end of instance C`), by
; its head for a unit (`C Int`); `class C` never reaches it
(instance name: (name) @name patterns: (_)? @unit.name) @definition.instance
(signature name: (prefix_id) @name) @definition.function
(function name: (prefix_id) @name) @definition.function
; an infix equation (`a <+> b = …`), named by its operator: it joins its
; `(<+>) ::` signature (the rule reads `(op)` and `op` as one name)
(function (infix operator: (operator) @name)) @definition.function
