import std/core/undiv
import std/core/unsafe

abstract value struct vslice<a>(vec: vector<a>, start: int, len: int)

pub fun vector/slice(v: vector<a>, start: int, len: int): vslice<a>
  Vslice(v, max(0, min(v.length, start)), max(0, min(v.length, len)))

pub fun vector/full/slice(v: vector<a>): vslice<a>
  Vslice(v, 0, v.length)

pub fun vector/start/slice(v: vector<a>, start: int): vslice<a>
  v.slice(start, v.length)

// TODO: is there a default argument qualifier? Like arg/len?
pub fun vslice/slice(v: vslice<a>, start: int, x/len: int): vslice<a>
  Vslice(v.vec, max(0, min(v.len, start)), max(0, min(v.len, x/len)))

pub fun vslice/start/slice(v: vslice<a>, start: int): vslice<a>
  v.slice(start, v.len)

pub fun vslice/at(slice: vslice<a>, i: int): maybe<a>
  slice.vec.at(slice.start + i)

pub fun vslice/is-empty(slice: vslice<a>): bool
  !slice.len.is-pos

pub fun vslice/next(slice: vslice<a>): maybe<(a,vslice<a>)>
  match slice.at(0)
    Nothing -> Nothing
    Just(a) -> Just((a, slice(start = slice.start + 1, len = slice.len - 1)))

// A single step in an iterative computation
pub value type loop<i,s,r>
  Loop(it: i, loop-state: s)
  Stop(ret: r)

// Do a tailrecursive loop over all elements in a vslice
// A loop-state can be passed from one step to the next
pub fun vslice/loop(slice: vslice<a>, loop-state: s, body: (maybe<(a,vslice<a>)>,s) -> e loop<vslice<a>,s,r>): e r
  match body(slice.next, loop-state)
    Stop(ret) -> ret
    Loop(it, next-loop-state) -> loop(pretend-decreasing(it), next-loop-state, body)

pub fun vslice/foreach(slice: vslice<a>, action: a -> e ()): e ()
  slice.loop(()) fn(it, _)
    match it
      Nothing -> Stop(())
      Just((x, xs)) -> 
        action(x)
        Loop(xs, ()) // TODO: why do we need to keep passing xs? I would like to automate that

pub fun vslice/foreach-indexed(slice: vslice<a>, action: (int, a) -> e ()): e ()
  slice.loop(0) fn(it, i)
    match it
      Nothing -> Stop(())
      Just((x, xs)) -> 
        action(i, x)
        Loop(xs, i + 1)

pub fun vslice/starts-with(haystack: vslice<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  if needle.len > haystack.len then
    False
  else
    val result = for-while(needle.len) fn(i)
      if needle.at(i) == haystack.at(i) then
        Nothing
      else
        Just(False)
    result.default(True)

pub fun vslice/contains(haystack: vslice<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  val result = for-while(haystack.len - needle.len + 1) fn(i)
    if haystack.slice(i).starts-with(needle) then
      Just(True)
    else
      Nothing
  result.default(False)

// Implementation without loop
// fun vslice/foreach( slice: vslice<a>, action: a -> e () ): e ()
//   match slice.next
//     Nothing -> ()
//     Just((x, xs)) -> 
//       action(x)
//       foreach(pretend-decreasing(xs), action)
