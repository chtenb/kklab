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
      

pub alias sample-space<a> = list<(float64, a)>

fun gen/choose(cases: sample-space<a>): <random> a
  assert("list needs to be non-empty", 0 < cases.length)
  val total = cases.map(fst).sum()
  val sample = uni/float64() * total
  var cumulative := 0.0
  val item = cases.find fn((p, _))
    cumulative := cumulative + p
    sample <= cumulative
  val last = unsafe-no-exn({cases.last.unjust})
  item.default(last).snd

pub effect gen<a>
  ctl outcome(value : a) : ()
  ctl weighted-outcome(w : float64, value : a) : ()

pub fun sample(action : () -> <gen<a>> ()) : <random> a
  val gen = build(action)
  gen/choose(gen)
  
pub fun weighted/subspace(weight : int, action : () -> <gen<a>> ()) : <gen<a>> ()
  val subspace = build(action)
  val size = subspace.length
  subspace.foreach fn((w, value))
    weighted-outcome(w / size.float64 * weight.float64, value)
    
pub fun subspace(action : () -> <gen<a>> ()) : <gen<a>> ()
  weighted/subspace(1, action)
  
pub fun gen/build(action : () -> <gen<a>> ()) : sample-space<a>
  with pretend-no-div()
  var outcomes := Nil
  handler
    ctl outcome(value)
      outcomes := Cons((1.0, value), outcomes)
      resume(())
    ctl weighted-outcome(w, value)
      outcomes := Cons((w, value), outcomes)
      resume(())
  action()
  outcomes
  
fun gen/int(lower: int, upper: int) : <random> int
  val x = uni/int(lower, upper)
  sample
    subspace
      [-1, 0, 1, lower, lower + 1, upper - 1, upper - 2].foreach fn(i)
        when(lower <= i && i <= upper)
          outcome(i)
    subspace(2)
          outcome(x)
  

// fun fixed/gen/list(n: int, gen-elem: () -> <random> a): <random> list<a>
//   if n == 0 then
//     []
//   else
//     Cons(gen-elem(), fixed/gen/list(pretend-decreasing(n - 1), gen-elem))
  
// fun bounded/gen/list(max-size: int, gen-elem: () -> <random> a): <random> list<a>
//   fixed/gen/list(gen/int(max-size), gen-elem)
  
