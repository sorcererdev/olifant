; Olifant's own (not vendored): an import statement, whole. locals.scm
; captures imported names, and misses a grouped `use crate::{a::B};`.
(use_declaration) @import
(extern_crate_declaration) @import
