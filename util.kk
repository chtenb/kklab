import std/num/float64
import std/core/unsafe
import std/core/undiv

fun panic(msg: string): a
  assert(msg, False)
  panic(pretend-decreasing(msg)) // Infinite loop to make the return type correct
  
fun unsafe-no-exn(f: () -> <exn|e> a, ?kk-file-line: string): e a
  with handler
    final ctl throw-exn(exn : exception)
      panic("Expected no exception at " ++ kk-file-line ++ ": but got " ++ exn.message)
  f()

pub fun todo() : a
  panic("todo")
  
pub fun last(v : vector<a>) : exn a
  v[v.length - 1]

pub fun min-by-exn(l : list<a>, ordering : a -> float64) : exn a
  match l
    Nil -> throw("empty list")
    Cons(x, xs) -> xs.min-by-rec(ordering, ordering(x), x)
  
pub fun min-by(l : list<a>, ordering : a -> float64, default : a) : a
  min-by-rec(l, ordering, flt-max, default)
  
fun min-by-rec(l : list<a>, ordering : a -> float64, smallest : float64, result : a) : a
  match l
    Nil -> result
    Cons(x, tail) -> 
      if ordering(x) < smallest then 
        min-by-rec(tail, ordering, ordering(x), x)
      else
        min-by-rec(tail, ordering, smallest, result)

pub fun vector/any(v : vector<a>, pred : a -> bool) : _ bool
  v.any-rec(pred, 0)
  
pub fun vector/any-rec(v : vector<a>, pred : a -> bool, i : int) : _ bool
  if v.length <= i then
    False
  else
    if pred(v[i]) then True else v.any-rec(pred, i+1)
    
pub fun vector/all(v : vector<a>, pred : a -> bool) : _ bool
  v.all-rec(pred, 0)
  
pub fun vector/all-rec(v : vector<a>, pred : a -> bool, i : int) : _ bool
  if v.length <= i then
    True
  else
    if !pred(v[i]) then False else v.all-rec(pred, i+1)

pub fun vector/index-of(v : vector<a>, pred : a -> bool) : _ int
  v.index-of-rec(pred, 0)

fun vector/index-of-rec(v : vector<a>, pred : a -> bool, i : int) : _ int
  if v.length <= i then
    -1
  else
    if pred(v[i]) then i else v.index-of-rec(pred, i+1)

pub fun vector/sum(v : vector<int>) : _ int
  v.sum-rec(0)

fun vector/sum-rec(v : vector<int>, i : int) : _ int
  if v.length <= i then
    0
  else
    v[i] + v.sum-rec(i+1)

pub fun vector/minimum(v : vector<int>) : _ int
  v.minimum-rec(0, 9999999)

fun vector/minimum-rec(v : vector<int>, i : int, m : int) : _ int
  if v.length <= i then
    m
  else
    vector/minimum-rec(v, i+1, min(v[i], m))

pub fun vector/empty() : vector<a>
  unit/vector()

pub fun vector/singleton(elem : a) : vector<a>
  vector(1, elem)
  
pub fun vector/unzip(v : vector<(a,b)>) : _ (vector<a>, vector<b>)
  // val first = vector-init-total(v.length, fn(i) unsafe-total({v[i].fst}))
  // val second = vector-init-total(v.length, fn(i) unsafe-total({v[i].snd}))
  val first = vector-init(v.length, fn(i) v[i].fst)
  val second = vector-init(v.length, fn(i) v[i].snd)
  (first, second)

// pub fun vector/unzip-rec(v : vector<(a,b)>, first : ref<h,vector<a>>, second : ref<h,vector<b>>, i : int) : _ (vector<a>, vector<b>)
//   if v.length <= i
//   then (first, second)
//   else
//     val (e1, e2) = v[i]
//     first[i] := e1
//     second[i] := e2
//     v.unzip-rec(first, second, i+1)

pub fun vector/append(first : vector<a>, second : vector<a>) : _ vector<a>
  // vector-init-total(first.length + second.length, fn(i) unsafe-total({index-two(first, second, i)}))
  vector-init(first.length + second.length, fn(i) index-two(first, second, i))
  
fun vector/index-two(first : vector<a>, second : vector<a>, i: int) : _ a
  if i < first.length then first[i] else second[i]

pub fun merge-sort(l : list<a>, at-most : (a, a) -> bool) : list<a>
  match l
    Nil -> Nil
    Cons(x, Nil) -> [x]
    _ ->
      val n = l.length / 2
      val (xs,ys) = l.split(n)
      val sorted-xs = merge-sort(pretend-decreasing(xs), at-most)
      val sorted-ys = merge-sort(pretend-decreasing(ys), at-most)
      sorted-merge(sorted-xs, sorted-ys, at-most)

fun sorted-merge(l1 : list<a>, l2 : list<a>, at-most : (a, a) -> bool) : list<a>
  match l1
    Nil -> l2
    Cons(x, xs) -> match l2
      Nil -> l1
      Cons(y, ys) ->
        if at-most(x, y) then
          Cons(x, sorted-merge(xs, l2, at-most))
        else
          Cons(y, sorted-merge(l1, ys, at-most))

fun uniq(sorted-list : list<a>, eq : (a, a) -> bool) : list<a>
  val dedupped = sorted-list.foldl([]) fn(acc, x) 
    match acc
      Nil -> Cons(x, Nil)
      Cons(prev, _) -> if eq(prev, x) then acc else Cons(x, acc)
  dedupped.reverse

// OUTDATED

fun vector-init-e(n : int, g : int -> e a) : e vector<a>
  list(0, n - 1).map(g).vector

fun lls/show(lls : list<list<string>>) : string
  lls.map(fn(l) l.join(", ")).join("\n")
  
fun lc/show(chars : list<char>) : string
  chars.string.show
    
fun show-maybe(x : maybe<a>, show : a -> string, default : string) : string
  match x
    Nothing -> default
    Just(y) -> show(y)

fun mi/eq(ma : maybe<int>, mb : maybe<int>) : bool
  match ma
    Nothing -> mb.is-nothing
    Just(a) -> match mb
      Nothing -> False
      Just(b) -> a == b

