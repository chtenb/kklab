pub fun list/at(l : list<a>, index : int) : maybe<a>
  fun rec(xs : list<a>, i : int) : maybe<a>
    match xs
      Nil -> Nothing
      Cons(x, xs) -> if i == 0 then Just(x) else rec(xs, i - 1)
  if index < 0 then Nothing else rec(l, index)
