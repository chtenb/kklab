import std/num/float64

fun merge-sort(l : list<a>, at-most : (a, a) -> bool) : list<a>
  match l
    Nil -> Nil
    Cons(x, Nil) -> [x]
    _ ->
      val n = l.length / 2
      val (xs,ys) = l.split(n)
      val sorted-xs = merge-sort(unsafe-decreasing(xs), at-most)
      val sorted-ys = merge-sort(unsafe-decreasing(ys), at-most)
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
