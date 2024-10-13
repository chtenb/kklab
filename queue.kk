import std/core/list

// Invariant: front.is-nil implies back.is-nil
// This is to enable constant time peek operation
abstract value struct queue<a>(front : list<a>, back : list<a>)

pub fun empty() : queue<a>
  Queue([], [])
  
pub fun is-empty(queue : queue<a>) : bool
  queue.front.is-nil
  
pub fun push(queue : queue<a>, item : a) : queue<a>
  if queue.is-empty then
    Queue([item], [])
  else
    Queue(queue.front, Cons(item, queue.back))
  
pub fun pop(queue : queue<a>) : maybe<(a, queue<a>)>
  match queue.front
    Cons(x, xs) ->
      val new-queue = if xs.is-nil then
        Queue(queue.back.reverse, [])
      else
        Queue(xs, queue.back)
      Just((x, new-queue))
    Nil -> Nothing

pub fun peek(queue : queue<a>) : maybe<a>
  queue.front.head

pub fun length(queue : queue<a>) : int
  queue.front.length + queue.back.length

pub fun show(queue : queue<a>, ?show : a -> string) : string
  "queue" ++ (queue.front ++ queue.back.reverse).show
