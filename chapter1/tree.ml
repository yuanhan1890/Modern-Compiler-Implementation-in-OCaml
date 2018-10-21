module type TreeBalanced = sig
type key
type 'a node
type 'a tree

val empty: 'a tree
val insert: key -> 'a -> 'a tree -> 'a tree
val delete: key -> 'a tree -> 'a tree
val lookup: key -> 'a tree -> 'a option
val member: key -> 'a tree -> bool
val invariant: 'a tree -> bool

end
