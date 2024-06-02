import std/core/undiv
import std/core/unsafe
import util
import vslice

alias slice = vslice<int>

ref type simple-trie<a>
  SimpleLookupLeaf(value : a)
  SimpleLookupNode(value : maybe<a>, children : vector<maybe<simple-trie<a>>>)

fun get-root-value(lookup : simple-trie<a>) : maybe<a>
  match lookup
    SimpleLookupLeaf(x) -> Just(x)
    SimpleLookupNode(mx, _) -> mx

fun simple/get-value(lookup : simple-trie<a>, p : slice) : exn maybe<a>
  match p.next
    Nothing -> lookup.get-root-value // Pattern exhausted, return current root value
    Just((k, ks)) ->
      match lookup
        SimpleLookupLeaf(_) -> Nothing // Reached bottom of tree before exhausting the pattern
        SimpleLookupNode(_, children) -> match children[k]
          Nothing -> Nothing
          Just(child) -> get-value(child, pretend-decreasing(ks))

fun simple/get-value'(lookup : simple-trie<a>, p : slice) : exn maybe<a>
  loop(p, get-value-rec, lookup, ())

fun simple/get-value-rec(p : maybe<(int,slice)>, lookup : simple-trie<a>, _ : ()) : exn tailrec<slice,simple-trie<a>,maybe<a>>
  match p
    Nothing -> Stop(lookup.get-root-value) // Pattern exhausted, return current root value
    Just((k, ks)) ->
      match lookup
        SimpleLookupLeaf(_) -> Stop(Nothing) // Reached bottom of tree before exhausting the pattern
        SimpleLookupNode(_, children) -> match children[k]
          Nothing -> Stop(Nothing)
          Just(child) -> Loop(ks, child)



ref type mut-trie<h,a>
  MutLookupNode(value : ref<h,maybe<a>>, children : vector<ref<h, maybe<mut-trie<h,a>>>>)

fun mut/get-value(lookup : mut-trie<h,a>, p : slice) : <read<h>,div,exn> maybe<a>
  match p.next
    Nothing -> match lookup
      MutLookupNode(value, _) -> !value
    Just((k, ks)) ->
      match lookup
        MutLookupNode(_, children) -> match !children[k]
          Nothing -> Nothing
          Just(child) -> get-value(child, ks)

fun insert(lookup : mut-trie<h, a>, p : slice, value : a) : <st<h>,exn,div> ()
  match p.next
    Nothing -> match lookup
      MutLookupNode(r, _) -> r := Just(value)
    Just((k, ks)) ->
      match lookup
        MutLookupNode(_, children) -> 
          val childref = children[k]
          match !childref
            Just(child) -> insert(child, ks, value)
            Nothing -> childref := Just(build-mut-trie-path(children.length, ks, value))

// Build a new trie with a path to a given value
fun build-mut-trie-path(alphabet-size : int, p : slice, value : a) : <st<h>,exn> mut-trie<h,a>
  val new-vector = vector-init-e(alphabet-size, fn(_) ref(Nothing))
  match p.next
    Nothing -> MutLookupNode(ref(Just(value)), new-vector)
    Just((k, ks)) ->
      val child = build-mut-trie-path(alphabet-size, pretend-decreasing(ks), value)
      val childref = new-vector[k]
      childref := Just(child)
      MutLookupNode(ref(Nothing), new-vector)

fun empty-mut-trie(alphabet-size : int)
  val new-vector = vector-init-e(alphabet-size, fn(_) ref(Nothing))
  MutLookupNode(ref(Nothing), new-vector)

fun traverse(lookup : mut-trie<h,a>, action : a -> <read<h>,div,exn|e> ()) : <read<h>,div,exn|e> ()
  match lookup
    MutLookupNode(value, children) -> 
      match !value
        Nothing -> ()
        Just(x) -> action(x)
      children.foreach fn(childref)
        match !childref
          Nothing -> ()
          Just(child) -> traverse(child, action)

