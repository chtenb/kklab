import std/core/unsafe
import std/core/undiv

// A stream effect where items can be yielded one at a time
pub effect yield<a>
  ctl yield(elem : a) : ()

pub fun yield/empty() : yield<t> ()
  ()

pub fun yield/list(it : () -> <yield<a>> ()) : list<a>
  with pretend-no-div()
  var result := Nil
  with fun yield(elem)
    result := Cons(elem, result)
  it()
  reverse(result) // List was accumulated in reverse order

pub fun yield/take(n : int, it : () -> <div,yield<a>> ()) : yield<a> ()
  var count := 0
  with ctl yield(elem)
    if count < n then
      yield(elem) // Forward the yield to our new yieldator
      count := count + 1
      resume(())
    else
      ()
  with pretend-no-div()
  it()

pub fun test-yield() : div list<int>
  fun fib-yield(n1 : int, n2 : int) : <div,yield<int>> ()
    val n3 = n1 + n2
    yield(n3)
    fib-yield(n2, n3)
    
  list
    take(100)
      fib-yield(1,1)


pub fun yield/foreach(it : () -> <yield<a>|e> (), do : a -> e ()) : e ()
  with ctl yield(elem)
    do(elem) // Run the foreach block for each element
    resume(()) // We don't want just the first element, so return to call site
  it() // Run yieldator

pub fun to-list2(it : () -> yield<a> ()) : list<a>
  with pretend-no-div() // No idea why this is necessary
  var result := Nil
  foreach(it) fn(elem)
    result := Cons(elem, result)
  reverse(result)

