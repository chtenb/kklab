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

// pub alias space<a> = () -> <gen<a>> ()
pub alias gen<a> = <div,random,outcome<a>>

pub div effect outcome<a>
  ctl outcome(value : a) : ()
  ctl subspace(s : () -> <div,random,outcome<a>> ()) : ()
  
pub fun sample(space : () -> gen<a> ()) : <div,random> a
  // with pretend-no-div()
  var outcomes := Nil
  var subspaces := Nil
  val h = handler
    ctl outcome(value)
      outcomes := Cons(value, outcomes)
      resume(())
    ctl subspace(g)
      subspaces := Cons(g, subspaces)
      resume(())
  h(space)
  val outcomesCount = outcomes.length
  val subspacesCount = subspaces.length
  val totalCount = outcomesCount + subspacesCount
  val i = uni/int(totalCount)
  match outcomes.at(i)
    Just(outcome) -> outcome
    Nothing ->
      val subspace = unsafe-no-exn
        subspaces.at(outcomesCount + i).unjust
      sample(subspace)

fun weighted/outcome(w : int, value : a) : <gen<a>> ()
  for(1, w) fn(_)
    outcome(value)
    
fun weighted/subspace(w : int, space : () -> gen<a> ()) : <gen<a>> ()
  for(1, w) fn(_)
    subspace(space)
  

    
fun positive/gen/int(upper: int) : <gen<int>> ()
  gen/int(0, upper)

fun gen/int(lower: int, upper: int) : <gen<int>> ()
  val x = uni/int(lower, upper)
  subspace
    [-1, 0, 1, lower, lower + 1, upper - 1, upper - 2].foreach fn(i)
      when(lower <= i && i <= upper)
        outcome(i)
  subspace
    outcome(x)
  

    
fun fixed/gen/list(n: int, gen-elem: () -> <gen<a>> ()): <div,random> list<a>
  if n <= 0 then
    []
  else
    Cons(sample(gen-elem), fixed/gen/list(pretend-decreasing(n - 1), gen-elem))
  
fun bounded/gen/list(max-size: int, gen-elem: () -> <gen<a>> ()): <div,random> list<a>
  val size = sample
    positive/gen/int(max-size)
  fixed/gen/list(size, gen-elem)
  
