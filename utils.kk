import std/num/float64

fun todo() : exn a
  throw("Not implemented", ExnTodo)

fun debug(msg : string) : total ()
  unsafe-total({println(msg)})

fun show-maybe(x : maybe<a>, show : a -> string, default : string) : string
  match x
    Nothing -> default
    Just(y) -> show(y)

fun min-by(l : list<a>, ordering : a -> float64, default : a) : a
  min-by-rec(l, ordering, default, flt-min, default)
  
fun min-by-rec(l : list<a>, ordering : a -> float64, default : a, smallest : float64, result : a) : a
  match l
    Nil -> result
    Cons(x, tail) -> 
      if ordering(x) < smallest then 
        min-by-rec(tail, ordering, default, ordering(x), x)
      else
        min-by-rec(tail, ordering, default, smallest, result)


fun eq(ma : maybe<int>, mb : maybe<int>) : bool
  match ma
    Nothing -> mb.is-nothing
    Just(a) -> match mb
      Nothing -> False
      Just(b) -> a == b

fun last(v : vector<a>) : exn a
  v[v.length - 1]

fun vector-init-e(n : int, g : int -> e a) : e vector<a>
  list(0, n - 1).map(g).vector

fun show(lls : list<list<string>>) : string
  lls.map(fn(l) l.join(", ")).join("\n")
