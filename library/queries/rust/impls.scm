; Olifant's own (not vendored): an inherent impl, named by its type, as a
; definition. tags.scm tags impls as @reference.implementation, names a
; trait impl by its trait, and skips a generic `impl<T> Store<T>`.
; A trait impl (`impl Trait for Type`) is not captured: no phrase names it.
(impl_item
  !trait
  type: (type_identifier) @name) @definition.impl

(impl_item
  !trait
  type: (generic_type
    type: (type_identifier) @name)) @definition.impl
