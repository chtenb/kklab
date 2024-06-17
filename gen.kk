import util
import std/core/undiv
import std/core/unsafe
import std/num/random
import std/num/int32
import std/num/float64
import std/core/debug

fun rnd-int32(lower: int, upper: int) : <random> int
  val i32 : int = random-int32().int
  ((i32 - lower) % (upper - lower)) + lower
  
fun rnd-float64(lower: float64, upper: float64) : <random> float64
  val f64 : float64 = random-float64()
  (f64 - lower) * (upper - lower) + lower

fun rnd-bool(): <random> bool
  random-bool()

fun rnd-int(lower: int, upper: int): <random> int
  rnd-nat(upper - lower) + lower

fun upper/rnd-nat(upper: int): <random> int
  assert("upper needs to be positive", upper >= 0)
  todo()

fun unbounded/rnd-nat(): <random> int
  fun rec(to-add: int): <random> int
    val i32 = random-int32().int
    if rnd-bool() then
      to-add + i32
    else
      val next-to-add = (i32 + to-add) * 2^32
      rec(pretend-decreasing(next-to-add))
  
  rec(0)


  
// Generate True with probability k/n
fun kn/gen-bool(k: int, n: int) : <random> bool
  rnd-int32(0, n) < k
  
// Generate True with probability 1/n
fun n/gen-bool(n: int) : <random> bool
  gen-bool(1, n)
  
// Generate True with probability 1/2
fun gen-bool() : <random> bool
  gen-bool(1, 2)

fun upper/gen-nat(upper: int): <random> int
  if gen-bool(10) then
    0
  else if gen-bool(10) then
    1
  else
    rnd-nat(upper)

fun unbounded/gen-nat(): <random> int
  if gen-bool(10) then
    0
  else if gen-bool(10) then
    1
  else
    rnd-nat()
  
fun choose(elems: vector<a>): <random> a
  val i = rnd-int32(0, elems.length)
  with unsafe-no-exn
  elems[i]

fun fixed/gen-list(n: int, gen-elem: () -> <random> a): <random> list<a>
  if n == 0 then
    []
  else
    Cons(gen-elem(), fixed/gen-list(pretend-decreasing(n - 1), gen-elem))
  
fun upper/gen-list(max-size: int, gen-elem: () -> <random> a): <random> list<a>
  fixed/gen-list(gen-nat(max-size), gen-elem)
  

fun main()
  val seed = srandom-int32().int
  println("Generated seed " ++ seed.show)
  with pseudo-random(seed)
  println(gen-bool(10).show)
  println(choose([1,2,3].vector).show)
  println(rnd-nat().show)
