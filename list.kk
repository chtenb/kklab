pub fun at(l : list<a>, index : int) : maybe<a>
  fun rec(xs : list<a>, i : int) : maybe<a>
    match xs
      Nil -> Nothing
      Cons(x, xs) -> if i == 0 then Just(x) else rec(xs, i - 1)
  if index < 0 then Nothing else rec(l, index)
  
pub fun remove-at(l : list<a>, index : int) : list<a>
  fun rec(xs : list<a>, i : int) : list<a>
    match xs
      Nil -> Nil
      Cons(x, xs) -> if i == 0 then rec(xs, i - 1) else Cons(x, rec(xs, i - 1))
  if index < 0 then l else rec(l, index)


fun for(low: int, high: int, f: int -> list<b>): list<b>
  list(low, high).flatmap(f)

fun pure(a: a): list<a>
  [a]

fun guard(a : bool, f: () -> list<a>): list<a>
  if a then f() else []
