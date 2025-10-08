
pub fun poset/(==)(x : a, y : a, ?(<=) : (a, a) -> bool) : bool
  x <= y && y <= x

pub fun foo(x : a, y : a, ?(==) : (a,a) -> bool, ?show : a -> string) : _ ()
  trace-show(x)
  trace-show(y)
  trace-show(x == y)

pub fun bar(x : a, y : a, ?cmp : (a,a) -> order, ?show : a -> string) : _ ()
  trace-show(x)
  trace-show(y)
  trace-show(x == y)

fun main()
  // foo('a', 'a')
  bar('a', 'a')
