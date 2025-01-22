import util
import list
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
  
  
// Choose an element in a list with uniform probability
fun uni/choose(cases: list<a>): <random> a
  assert("list needs to be non-empty", 0 < cases.length)
  val i = uni/int(cases.length)
  with unsafe-no-exn
  cases.at(i).unjust
