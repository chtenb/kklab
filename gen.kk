import util
import std/core/vector
import std/core/undiv
import std/core/unsafe
import std/num/random
import std/num/int32
import std/num/float64
import std/core/debug

///////////
/// UNI ///
///////////

// Return a random integer in the range [lower, upper) and [0, 2^31).
// TODO: remove outer range limit
fun uni/int(lower: int, upper: int) : <random> int
  assert("lower must be less than upper", lower <= upper)
  val i32 : int = random-int32().int
  ((i32 - lower) % (upper - lower)) + lower

// Return a random integer in the range [0, upper) and [0, 2^31).
fun positive/uni/int(upper: int): <random> int
  assert("upper needs to be positive", 0 <= upper)
  uni/int(0, upper)
  
// Return a random integer in the range [-2^32, 2^31).
fun any/uni/int(): <random> int
  random-int32().int

  
// Return a float64 integer in the range [lower, upper).
fun uni/float64(lower: float64, upper: float64) : <random> float64
  assert("lower must be less than upper", lower <= upper)
  val f64 : float64 = random-float64()
  (f64 - lower) * (upper - lower) + lower
  
// Return a float64 integer in the range [0, upper).
fun positive/uni/float64(upper: float64) : <random> float64
  assert("upper needs to be positive", 0.0 <= upper)
  uni/float64(0.0, upper)
  
// Return a float64 integer in the range [0, 1).
fun unit/uni/float64() : <random> float64
  random-float64()
  
  
// Choose an element in a vector with uniform probability
fun uni/choose(cases: vector<a>): <random> a
  assert("vector needs to be non-empty", 0 < cases.length)
  val i = uni/int(cases.length)
  with unsafe-no-exn
  cases[i]

// Choose a generator in a vector with uniform probability, and run it
fun block/uni/choose(cases: vector<() -> <random> a>): <random> a
  assert("vector needs to be non-empty", 0 < cases.length)
  val i = uni/int(cases.length)
  with unsafe-no-exn
  cases[i]()
 
  
///////////
/// GEN ///
///////////

// Generate True with probability k/n
fun k/n/gen/bool(k: int, n: int) : <random> bool
  uni/int(0, n) < k
  
// Generate True with probability 1/n
fun n/gen/bool(n: int) : <random> bool
  gen/bool(1, n)
  
// Generate True with probability 1/2
fun gen/bool() : <random> bool
  gen/bool(1, 2)
  
fun gen/int(lower: int, upper: int) : <random> int
  uni/int(lower, upper)
  
fun positive/gen/int(upper: int) : <random> int
  uni/int(upper)
      

// Choose a case in a vector with uniform probability
fun gen/choose(cases: vector<(float64, a)>): <random> a
  assert("vector needs to be non-empty", 0 < cases.length)
  val sample = uni/float64()
  var cumulative := 0.0
  val item = cases.find fn((p, _))
    cumulative := cumulative + p
    sample <= cumulative
  val last = unsafe-no-exn({cases.last()})
  item.default(last).snd


fun fixed/gen/list(n: int, gen-elem: () -> <random> a): <random> list<a>
  if n == 0 then
    []
  else
    Cons(gen-elem(), fixed/gen/list(pretend-decreasing(n - 1), gen-elem))
  
fun bounded/gen/list(max-size: int, gen-elem: () -> <random> a): <random> list<a>
  fixed/gen/list(gen/int(max-size), gen-elem)
  

fun main()
  val seed = srandom-int32().int
  println("Generated seed " ++ seed.show)
  with pseudo-random(seed)
  println(gen/choose([(0.1, "x"), (0.9, "y")].vector).show)
