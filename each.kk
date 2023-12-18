

pub effect each
  ctl each(elems : list<a>) : a
  ctl fail() : a

pub fun list-comp(action : () -> each a) : list<a>
  with handler
    return (x) [x]
    ctl each(elems) elems.flatmap(resume)
    ctl fail() []
  action()

pub fun guard(pred : bool) : each ()
  if pred then () else fail()

pub fun test-each()
  list-comp
    val x = each([1,2,3])
    val y = each([1,2,2])
    val z = x+y
    guard (z != 3)
    z * z

pub effect yield<a>
  ctl yield(elem : a) : ()

pub fun to-list(it : () -> yield<a> ()) : list<a>
  with unsafe-no-div()
  var result := Nil
  with fun yield(elem)
    result := Cons(elem, result)
  it()
  reverse(result) // List was accumulated in reverse order

pub fun take(n : int, it : () -> <div,yield<a>> ()) : yield<a> ()
  var count := 0
  with ctl yield(elem)
    if count < n then
      yield(elem) // Forward the yield to our new yieldator
      count := count + 1
      resume(())
    else
      ()
  with unsafe-no-div()
  it()

pub fun test-yield() : div list<int>
  fun fib-yield(n1 : int, n2 : int) : <div,yield<int>> ()
    val n3 = n1 + n2
    yield(n3)
    fib-yield(n2, n3)
    
  to-list
    take(100)
      fib-yield(1,1)

    

pub fun foreach(it : () -> <yield<a>|e> (), do : a -> e ()) : e ()
  with ctl yield(elem)
    do(elem) // Run the foreach block for each element
    resume(()) // We don't want just the first element, so return to call site
  it() // Run yieldator

pub fun to-list2(it : () -> yield<a> ()) : list<a>
  with unsafe-no-div() // No idea why this is necessary
  var result := Nil
  foreach(it) fn(elem)
    result := Cons(elem, result)
  reverse(result)
