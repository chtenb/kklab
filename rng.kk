import util
import list
import std/core/undiv
import std/core/unsafe
import std/num/random
import std/num/int32
import std/num/float64
import std/core/debug


// Return a random integer in the range [lower, upper) and [0, 2^31).
// TODO: remove outer range limit
fun uniform/int(lower: int, upper: int) : <random> int
  assert("lower must be less than upper", lower <= upper)
  val i32 : int = random-int32().int
  ((i32 - lower) % (upper - lower)) + lower

// Return a random integer in the range [0, upper) and [0, 2^31).
fun positive/uniform/int(upper: int): <random> int
  assert("upper needs to be positive", 0 <= upper)
  uniform/int(0, upper)
  
// Return a random integer in the range [-2^32, 2^31).
fun any/uniform/int(): <random> int
  random-int32().int

  
// Return a float64 integer in the range [lower, upper).
fun uniform/float64(lower: float64, upper: float64) : <random> float64
  assert("lower must be less than upper", lower <= upper)
  val f64 : float64 = random-float64()
  (f64 - lower) * (upper - lower) + lower
  
// Return a float64 integer in the range [0, upper).
fun positive/uniform/float64(upper: float64) : <random> float64
  assert("upper needs to be positive", 0.0 <= upper)
  uniform/float64(0.0, upper)
  
// Return a float64 integer in the range [0, 1).
fun unit/uniform/float64() : <random> float64
  random-float64()
  
  
// Choose an element in a list with uniform probability
fun uniform/choose(cases: list<a>): <random> a
  assert("list needs to be non-empty", 0 < cases.length)
  val i = uniform/int(cases.length)
  with unsafe-no-exn
  cases.at(i).unjust
