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

pub fun range/list(n: int): list<int>
  range/list(0, n - 1)

pub fun for(low: int, high: int, f: int -> list<b>): list<b>
  list(low, high).flatmap(f)

pub fun pure(a: a): list<a>
  [a]

pub fun guard(a : bool, f: () -> list<a>): list<a>
  if a then f() else []

pub fun trim-start(l : list<a>, trim : list<a>, ?(==) : (a,a) -> bool) : list<a>
  l.drop-while(fn(elm) trim.find(fn(t) elm == t).is-just)

pub fun trim-end(l : list<a>, trim : list<a>, ?(==) : (a,a) -> bool) : list<a>
  l.reverse.trim-start(trim).reverse

pub fun dedup(l : list<a>, ?(==) : (a,a) -> bool) : list<a>
  var prev-elm := Nothing
  l.filter fn(elm)
    val duplicate = prev-elm == Just(elm)
    prev-elm := Just(elm)
    !duplicate

// Concatenate all lists in a list, separated by a sep
fun joinsep( xs : list<list<a>>, sep : list<a>) : list<a>
  match xs
    Nil -> []
    Cons(x,Nil) -> x
    Cons(x,xxs) -> x ++ sep ++ xxs.joinsep(sep)

