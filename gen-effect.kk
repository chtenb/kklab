import util
import list
import std/core/undiv
import std/core/unsafe
import std/num/random
import std/num/int32
import std/num/float64
import std/core/debug
import rng 
  
///////////
/// GEN ///
///////////


// This gen effect supports building sample spaces in an effectful style.
// Running the rng can be delayed until sample time, but not required.
// The downside is that effects do not compose well if you need the input of
// one space to build another. You need to call the `sample` function for this.


// pub alias space<a> = () -> gen<a> ()
pub alias gen<a> = <div,outcome<a>>

pub div effect outcome<a>
  ctl outcome(value : a) : ()
  ctl subspace(s : () -> <random,gen<a>> ()) : ()
  
pub fun sample(space : () -> <random,gen<a>> ()) : <div,random> a
  var outcomes := Nil
  var subspaces := Nil
  val h = handler
    ctl outcome(value)
      outcomes := Cons(value, outcomes)
      resume(())
    ctl subspace(s)
      subspaces := Cons(s, subspaces)
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
        subspaces.at(i - outcomesCount).unjust
      sample(subspace)

fun delay(r : () -> <random,div> a) : gen<a> ()
  subspace
    outcome(r())

fun bind(space : () -> gen<a> (), f : a -> gen<b> ()) : gen<b> ()
  subspace
    val a = sample(space)
    f(a)

fun weighted/outcome(w : int, value : a) : gen<a> ()
  for(w) fn(_)
    outcome(value)
    
fun weighted/subspace(w : int, space : () -> gen<a> ()) : gen<a> ()
  for(w) fn(_)
    subspace(space)
  

// Generate True with probability k/n
fun k/n/gen/bool(k: int, n: int) : gen<bool> ()
  delay
    uni/int(0, n) < k
  
// Generate True with probability 1/n
fun n/gen/bool(n: int) : gen<bool> ()
  gen/bool(1, n)
  
// Generate True with probability 1/2
fun gen/bool() : gen<bool> ()
  gen/bool(1, 2)

    
fun positive/gen/int(upper: int) : gen<int> ()
  gen/int(0, upper)

fun gen/int(lower: int, upper: int) : gen<int> ()
  subspace
    [-1, 0, 1, lower, lower + 1, upper - 1, upper - 2].foreach fn(i)
      when(lower <= i && i <= upper)
        outcome(i)
  subspace
    outcome(uni/int(lower, upper))
  
    
fun fixed/gen/list(length: int, gen-elem: () -> gen<a> ()): gen<list<a>> ()
  fun rec(n)
    if n <= 0 then
      []
    else
      Cons(sample(gen-elem), rec(pretend-decreasing(n - 1)))
  subspace
    outcome(rec(length))
  
fun bounded/gen/list(max-length: int, gen-elem: () -> gen<a> ()): gen<list<a>> ()
  with size <- (fn(){gen/int(max-length)}).bind
  fixed/gen/list(size, gen-elem)
  // subspace
  //   val size = sample
  //     gen/int(max-length)
  //   fixed/gen/list(size, gen-elem)
  
