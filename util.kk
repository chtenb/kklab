import std/num/float64
import std/core/unsafe
import std/core/undiv

// EXCEPTIONS
  
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

  
// BRANCHING
pub fun when(condition : bool, action : () -> e ()) : e ()
  if condition then action() else ()

pub fun foldl-while(l : list<a>, empty : b, f : (b, a) -> maybe<b>) : b
  match l
    Nil -> empty
    Cons(x, xs) -> match f(empty, x)
      Nothing -> empty
      Just(b) -> foldl-while(xs, b, f)

// ORDERING

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

  
  
// STRING

fun string/empty() : string
  ""

fun string/append(first : string, second : string) : string
  first ++ second



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

// REPL

pub fun vec(l : list<a>) : vector<a>
  l.vector
