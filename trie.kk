import utils

alias intstring = list<int>

ref type simple-trie<a>
  SimpleLookupLeaf(value : a)
  SimpleLookupNode(value : maybe<a>, children : vector<maybe<simple-trie<a>>>)

fun get-root-value(lookup : simple-trie<a>) : maybe<a>
  match lookup
    SimpleLookupLeaf(x) -> Just(x)
    SimpleLookupNode(mx, _) -> mx

fun get-value(lookup : simple-trie<a>, p : intstring) : exn maybe<a>
  match p
    Nil -> lookup.get-root-value // Pattern exhausted, return current root value
    Cons(k, ks) ->
      match lookup
        SimpleLookupLeaf(_) -> Nothing // Reached bottom of tree before exhausting the pattern
        SimpleLookupNode(_, children) -> match children[k]
          Nothing -> Nothing
          Just(child) -> get-value(unsafe-decreasing(child), ks)


ref type mut-trie<h,a>
  MutLookupNode(value : ref<h,maybe<a>>, children : vector<ref<h, maybe<mut-trie<h,a>>>>)

fun get-value(lookup : mut-trie<h,a>, p : intstring) : <read<h>,div,exn> maybe<a>
  match p
    Nil -> match lookup
      MutLookupNode(value, _) -> !value
    Cons(k, ks) ->
      match lookup
        MutLookupNode(_, children) -> match !children[k]
          Nothing -> Nothing
          Just(child) -> get-value(child, ks)

fun insert(lookup : mut-trie<h, a>, p : intstring, value : a) : <st<h>,exn,div> ()
  match p
    Nil -> match lookup
      MutLookupNode(r, _) -> r := Just(value)
    Cons(k, ks) ->
      match lookup
        MutLookupNode(_, children) -> 
          val childref = children[k]
          match !childref
            Just(child) -> insert(child, ks, value)
            Nothing -> childref := Just(build-mut-trie-path(children.length, ks, value))

// Build a new trie with a path to a given value
fun build-mut-trie-path(alphabet-size : int, p : intstring, value : a) : <st<h>,exn> mut-trie<h,a>
  val new-vector = vector-init-e(alphabet-size, fn(_) ref(Nothing))
  match p
    Nil -> MutLookupNode(ref(Just(value)), new-vector)
    Cons(k, ks) ->
      val child = build-mut-trie-path(alphabet-size, ks, value)
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

