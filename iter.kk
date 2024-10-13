import std/core/unsafe
import std/core/undiv
import yield

// Models an (effectful) iteration which can be advanced one step at a time
type iter<a,e>
  Iter(cont : () -> e maybe<(a,iter<a,e>)>)
  
fun empty() : iter<a,e>
  Iter({Nothing})

fun next(it : iter<a,e>) : e maybe<(a,iter<a,e>)>
  (it.cont)()

pub fun is-empty(it : iter<a,e>) : e bool
  it.next.is-nothing
  
pub fun list(it : iter<a,<div|e>>) : <div|e> list<a>
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

