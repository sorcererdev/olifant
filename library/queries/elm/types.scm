; Olifant's own (not vendored): Elm's type aliases and the items of its
; types. The upstream tags.scm names type declarations only.

; A type alias is a type definition; a record alias's definition is its
; record (so `record NAME` reaches it by its kind word), the whole alias
; its unit.
(type_alias_declaration
  name: (upper_case_identifier) @name
  typeExpression: (type_expression) @_type
  (#not-match? @_type "^[{]")) @definition.type

(type_alias_declaration
  name: (upper_case_identifier) @name
  typeExpression: (type_expression . (record_type) @definition.type .)) @class.outer

; The items: a type's variants, a record alias's own fields (not those of a
; record nested in a variant or a field).
(type_declaration unionVariant: (union_variant) @class.inner)

(type_alias_declaration
  typeExpression: (type_expression . (record_type fieldType: (field_type) @class.inner) .))
