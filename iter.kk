pub effect yield<a>
  ctl yield(elem : a) : ()

pub fun empty() : yield<t> ()
  ()

pub fun list(it : () -> <yield<a>> ()) : list<a>
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
    
  list
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

type iter<a,e>
  Iter(cont : () -> e maybe<(a,iter<a,e>)>)

fun next(it : iter<a,e>) : e maybe<(a,iter<a,e>)>
  (it.cont)()

pub fun is-empty(it : iter<a,e>) : e bool
  it.next.is-nothing
  
pub fun list(it : iter<a,e>) : e list<a>
  match it.next
    Nothing -> Nil
    Just((elem, rest)) -> Cons(elem, rest.list)

pub fun iter(g : () -> <yield<a>|e> ()) : iter<a,e>
  Iter({
    handle g
      return(_) Nothing
      ctl yield(a)
        Just((a, Iter(fn() resume(()))))
  })

