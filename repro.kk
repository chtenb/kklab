import std/num/float64
import std/core/undiv
import std/core/unsafe
import std/num/random

fun panic(msg: string): a
  assert(msg, False)
  panic(pretend-decreasing(msg)) // Infinite loop to make the return type correct
  
fun unsafe-no-exn(f: () -> <exn|e> a, ?kk-file-line: string): e a
  with handler
    final ctl throw-exn(exn : exception)
      panic("Expected no exception at " ++ kk-file-line ++ ": but got " ++ exn.message)
  f()

fun unit/uni/float64() : <random> float64
  random-float64()
 
fun choose(weighted-outcomes: list<(float64, a)>): <random> a
  assert("list needs to be non-empty", 0 < weighted-outcomes.length)
  val total = weighted-outcomes.map(fst).sum()
  val sample = uni/float64() * total
  var cumulative := 0.0
  val item = weighted-outcomes.find fn((p, _))
    cumulative := cumulative + p
    sample <= cumulative
  val last = unsafe-no-exn({weighted-outcomes.last.unjust})
  item.default(last).snd

pub effect gen<a>
  ctl outcome(value : a) : ()
  ctl weighted-outcome(w : float64, value : a) : ()

pub fun sample(action : () -> <gen<a>> ()) : <random> a
  val gen = build(action)
  choose(gen)
  
pub fun subspace(action : () -> <gen<a>> ()) : <gen<a>> ()
  val subspace = build(action)
  val size = subspace.length
  subspace.foreach fn((w, value))
    weighted-outcome(w / size.float64, value)
  
pub fun build(action : () -> <gen<a>> ()) : list<(float64, a)>
  with pretend-no-div()
  var tree := Nothing
  var outcomes := Nil
  with handler
    ctl outcome(value)
      outcomes := Cons((1.0, value), outcomes)
      resume(())
    ctl weighted-outcome(w, value)
      outcomes := Cons((w, value), outcomes)
      resume(())
  action()
  outcomes
