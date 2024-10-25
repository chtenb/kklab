import std/core/unsafe
import std/core/undiv
import queue
import vector

// Models a single (effectful) iteration which can be advanced one step at a time
type iter<a,e>
  Iter(cont : () -> e maybe<(a,iter<a,e>)>)
  
fun empty() : iter<a,e>
  Iter(fn() Nothing)

fun next(it : iter<a,e>) : e maybe<(a,iter<a,e>)>
  it.cont.()

pub fun is-empty(it : iter<a,e>) : e bool
  it.next.is-nothing
  
pub fun list(it : iter<a,<div|e>>) : <div|e> list<a>
  match it.next
    Nothing -> Nil
    Just((elem, rest)) -> Cons(elem, rest.list)

pub fun map(it : iter<a,e>, f : a -> e b) : iter<b,e>
  Iter
    match it.next
      Nothing -> Nothing
      Just((a, rest)) -> Just((f(a), map(pretend-decreasing(rest), f)))
        
pub fun foreach(it : iter<a,e>, do : a -> e ()) : e ()
  match it.next
    Nothing -> ()
    Just((a, rest)) -> 
      do(a)
      foreach(pretend-decreasing(rest), do)
        
pub fun foldl(it : iter<a,e>, empty : b, f : (b, a) -> e b) : e b
  match it.next
    Nothing -> empty
    Just((a, rest)) -> foldl(pretend-decreasing(rest), f(empty, a), f)
  
