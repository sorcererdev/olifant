; Olifant's own (not vendored): nvim-treesitter-textobjects' Haskell file
; marks no class bodies. A data type's constructors, a class's and an
; instance's declarations are their items. A function has no inner range:
; it is its own container and its one item (`end of` goes after it).
(data_type constructors: (data_constructors (data_constructor) @class.inner))
(class declarations: (class_declarations (_) @class.inner))
(instance declarations: (instance_declarations (_) @class.inner))
