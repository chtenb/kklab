import std/num/float64
import std/core/unsafe
import std/core/undiv

// FUNCTIONS
pub infixr 55 (>.)
fun (>.)(f : a -> e b, g : b -> e c) : (a -> e c)
  fn(x) g(f(x))

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

pub fun maybe/effect/default(m : maybe<a>, fallback : () -> e a) : e a
  match m
    Just(x) -> x
    Nothing -> fallback()

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

pub fun pre-order/sort(l : list<a>, ?(<=) : (a, a) -> bool) : list<a>
  match l
    Nil -> Nil
    Cons(x, Nil) -> [x]
    _ ->
      val n = l.length / 2
      val (xs,ys) = l.split(n)
      val sorted-xs = sort(pretend-decreasing(xs), ?(<=) = (<=))
      val sorted-ys = sort(pretend-decreasing(ys), ?(<=) = (<=))
      sorted-merge(sorted-xs, sorted-ys, (<=))

fun sorted-merge(l1 : list<a>, l2 : list<a>, (<=) : (a, a) -> bool) : list<a>
  match l1
    Nil -> l2
    Cons(x, xs) -> match l2
      Nil -> l1
      Cons(y, ys) ->
        if x <= y then
          Cons(x, sorted-merge(xs, l2, (<=)))
        else
          Cons(y, sorted-merge(l1, ys, (<=)))

pub fun uniq(sorted-list : list<a>, ?(==) : (a, a) -> bool) : list<a>
  val dedupped = sorted-list.foldl([]) fn(acc, x) 
    match acc
      Nil -> Cons(x, Nil)
      Cons(prev, _) -> if prev == x then acc else Cons(x, acc)
  dedupped.reverse

  
// STRING

pub fun string/empty() : string
  ""

pub fun string/append(first : string, second : string) : string
  first ++ second
  // concat/join([first,second])

pub fun string/unquote(s : string) : string
  if s.count > 2 && s.starts-with("'").is-just && s.ends-with("'").is-just then
    s.tail.truncate(s.count - 2)
  else
    s

pub fun spaces/string/indent(s : string, spaces : int) : string
  s.indent(" ".repeat(spaces))

pub fun string/indent(s : string, indentstring : string) : string
  s.split("\n").map(fn(line) indentstring ++ line).join("\n")


// LIST QUERIES

pub fun sort-by(l : list<a>, f : a -> b, ?(<=) : (b, b) -> bool) : list<a>
  l.sort(?(<=) = fn(x,y) f(x) <= f(y))

pub fun group-by(l : list<a>, f : a -> b, ?(<=) : (b, b) -> bool) : list<list<a>>
  pretend-no-div
    l.sort-by(f, ?(<=) = (<=)).group-sorted(f, ?(<=) = (<=))
  
fun group-sorted(l : list<a>, f : a -> b, ?(<=) : (b, b) -> bool) : div list<list<a>>
  var result := Nil
  var group := Nil
  l.foreach fn(x)
    match group
      Nil ->
        group := Cons(x, Nil)
        ()
      Cons(prev, _) -> 
        if f(x) <= f(prev) then
          group := Cons(x, group)
        else
          result := Cons(group, result)
          group := Nil
  result

pub fun (<=)(xs : list<a>, ys : list<a>, ?(<=) : (a,a) -> bool) : bool
  match xs
    Nil -> ys.is-empty
    Cons(x, xxs) -> match ys
      Nil -> True
      Cons(y, yys) -> x <= y && (y <= x).implies(xxs <= yys)

// TUPLES

pub fun tuple2/lexi/(<=)((x1,x2) : (a,b), (y1,y2) : (a,b), ?fst/(<=) : (a,a) -> bool, ?snd/(<=) : (b,b) -> bool) : bool
  x1 <= y1 && (y1 <= x1).implies(
    x2 <= y2
  )
  
pub fun tuple3/lexi/(<=)((x1,x2,x3) : (a,b,c), (y1,y2,y3) : (a,b,c), ?fst/(<=) : (a,a) -> bool, ?snd/(<=) : (b,b) -> bool, ?thd/(<=) : (c,c) -> bool) : bool
  x1 <= y1 && (y1 <= x1).implies(
    x2 <= y2 && (y2 <= x2).implies(
      x3 <= y3
    )
  )
  
pub fun tuple4/lexi/(<=)((x1,x2,x3,x4) : (a,b,c,d), (y1,y2,y3,y4) : (a,b,c,d), ?fst/(<=) : (a,a) -> bool, ?snd/(<=) : (b,b) -> bool, ?thd/(<=) : (c,c) -> bool, ?frt/(<=) : (d,d) -> bool) : bool
  x1 <= y1 && (y1 <= x1).implies(
    x2 <= y2 && (y2 <= x2).implies(
      x3 <= y3 && (y3 <= x3).implies(
        x4 <= y4
      )
    )
  )


// ALGEBRA

pub fun monoid/fold(l : list<a>, ?empty : a, ?append : (a,a) -> a) : a
  l.foldl(empty, append)

pub fun bool/implies(x : bool, y : bool) : bool
  !x || y

// pub fun poset/(==)(x : a, y : a, ?(<=) : (a, a) -> bool) : bool
//   x <= y && y <= x

// pub fun monoid/(++)(x : a, y : a, ?append : (a,a) -> a) : a
//   x.append(y)

// REPL

pub fun vec(l : list<a>) : vector<a>
  l.vector
