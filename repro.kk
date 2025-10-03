import std/core/undiv
import std/core/unsafe

// Merge sort can be done on any pre-order
pub fun pre-order/sort(l : list<a>, ?(<=) : (a, a) -> bool) : list<a>
  match l
    Nil -> Nil
    Cons(x, Nil) -> [x]
    _ ->
      val n = l.length / 2
      val (xs,ys) = l.split(n)
      val sorted-xs = sort(pretend-decreasing(xs), (<=))
      val sorted-ys = sort(pretend-decreasing(ys), (<=))
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

// But a total order also works of course
pub fun order/sort(l : list<a>, ?cmp : (a, a) -> order) : list<a>
  l.sort(fn(x,y) cmp(x,y).is-lt)

// We can define a left-to-right total order on vectors, if the elements are totally ordered
pub fun vector/cmp-lr(xs : vector<a>, ys : vector<a>, ?cmp : (a,a) -> order): order
  val x-len = xs.length
  val y-len = ys.length
  val min-len = min(x-len, y-len)
  val common-indices = range/list(0, min-len - 1)
  val result = common-indices.foreach-while fn(i)
    match cmp(xs.unsafe-at(i), ys.unsafe-at(i))
      Lt -> Just(Lt)
      Gt -> Just(Gt)
      Eq -> Nothing // continue

  result.default(cmp(x-len, y-len))


// ----- UTILS ------
pub fun unsafe-at(v : vector<a>, index : int) : a
  with unsafe-no-exn
  v[index]
  
fun unsafe-no-exn(f: () -> <exn|e> a, ?kk-file-line: string): e a
  with handler
    final ctl throw-exn(exn : exception)
      panic("Expected no exception at " ++ kk-file-line ++ ": but got " ++ exn.message)
  f()

fun panic(msg: string): a
  assert(msg, False)
  panic(pretend-decreasing(msg)) // Infinite loop to make the return type correct

pub fun vector/show( xs : vector<a>, ?show : a -> e string ) : e string
  "vec[" ++ xs.map(show).vectorsep/join(",") ++ "]"


// ----- MAIN ------
pub fun main()
  val labels = ["abc".vector, "abd".vector]
  println(labels.sort(cmp-lr).show) // How to call the sort function with the total order?
