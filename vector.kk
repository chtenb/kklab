import util
import int
import std/core/unsafe
import std/core/undiv
import std/num/float64

// Show a vector
pub fun show( xs : vector<a>, ?show : a -> e string ) : e string
  "vec[" ++ xs.map(show).join(",") ++ "]"

pub fun unsafe-at(v : vector<a>, index : int) : a
  with unsafe-no-exn
  v[index]

// Return the first element that matches the predicate.
fun find(v: vector<a>, pred: a -> e bool): e maybe<a>
  v.foreach-while fn(a)
    if pred(a) then Just(a) else Nothing

pub fun last(v : vector<a>) : exn a
  v[v.length - 1]

pub fun any(v : vector<a>, pred : a -> bool) : bool
  fun rec(i : int) : _ bool
    if v.length <= i then False
    else if pred(v.unsafe-at(i)) then True 
    else rec(pretend-decreasing(i+1))
  rec(0)
      
pub fun all(v : vector<a>, pred : a -> bool) : bool
  fun rec(i : int) : _ bool
    if v.length <= i then True
    else if !pred(v.unsafe-at(i)) then False 
    else rec(pretend-decreasing(i+1))
  rec(0)
  
pub fun index-of(v : vector<a>, pred : a -> bool) : int
  fun rec(i : int) : _ int
    if v.length <= i then -1
    else if pred(v.unsafe-at(i)) then i 
    else rec(pretend-decreasing(i+1))
  rec(0)

pub fun minimum(v : vector<int>) : _ int
  assert("vector must be non-empty", v.length > 0)
  fun rec(i : int, m : int) : _ int
    if v.length <= i then m
    else rec(i+1, min(v[i], m))
  rec(1, v[0])

pub val vector/empty : forall<a> vector<a> = unit/vector()

pub fun singleton(elem : a) : vector<a>
  vector(1, elem)

pub fun append(a : vector<a>, b : vector<a>) : _ vector<a>
  // vector-init-total(a.length + b.length, fn(i) unsafe-total({index-two(a, b, i)}))
  vector-init(a.length + b.length, fn(i) index-two(a, b, i))
  
fun index-two(first : vector<a>, second : vector<a>, i: int) : _ a
  if i < first.length then first[i] else second[i]
  
pub fun unzip(v : vector<(a,b)>) : _ (vector<a>, vector<b>)
  // val first = vector-init-total(v.length, fn(i) unsafe-total({v[i].fst}))
  // val second = vector-init-total(v.length, fn(i) unsafe-total({v[i].snd}))
  val first = vector-init(v.length, fn(i) v[i].fst)
  val second = vector-init(v.length, fn(i) v[i].snd)
  (first, second)

pub fun monoid/join(v : vector<m>, ?empty : m, ?append : (m, m) -> m) : m
  with pretend-no-div
  var result := @implicit/empty
  v.foreach fn(m)
    result := append(result, m)
  result

pub fun int/sum(v : vector<int>) : int
  // v.monoid/join(0, int/(+))
  v.monoid/join

pub fun float64/sum(v : vector<float64>) : float64
  v.monoid/join(0.0, float64/(+))
  
fun join(vs : vector<vector<a>>) : _ vector<a>
  vector-init(vs.map(length).sum(), fn(i) vs.index-nested(i))
  
fun index-nested(vs : vector<vector<a>>, i: int) : _ a
  var offset := 0
  vs.foreach-while fn(v)
    if i - offset < v.length then 
      Just(v[i]) 
    else 
      offset := offset + v.length
      Nothing
  panic("Index out of bounds")
