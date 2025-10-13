import std/core/undiv
import std/core/unsafe
import list
import util

import vector
import coka-std/std/core-extras

abstract value struct vslice<a>(parent_: vector<a>, start_: int, len_: int)
pub fun vslice/parent(v: vslice<a>): vector<a>
  v.parent_
pub fun vslice/start(v: vslice<a>): int
  v.start_
pub fun vslice/length(v: vslice<a>): int
  v.len_
pub fun vslice/end(v: vslice<a>): int
  v.start_ + v.len_

// Show a vslice
pub fun vslice/show(v: vslice<a>, ?show: a -> e string): e string
  "vslice[" ++ v.map(show).vectorsep/join(",") ++ "]"

pub fun vslice/repr(v: vslice<a>, ?show: a -> e string): e string
  "vslice(" ++ v.parent.show ++ ", " ++ v.start.show ++ ", " ++ v.length.show ++ ")"

pub fun vslice/show-bounds(v: vslice<a>): e string
  "vslice(" ++ v.start.show ++ ", " ++ v.length.show ++ ")"

pub fun vector/slice(v: vector<a>, start: int, len: int): vslice<a>
  Vslice(v, max(0, min(v.length, start)), max(0, min(v.length - start, len)))

pub fun vector/full/slice(v: vector<a>): vslice<a>
  Vslice(v, 0, v.length)

pub fun vector/start/slice(v: vector<a>, start: int): vslice<a>
  v.slice(start, v.length)

pub fun vector/len/slice(v: vector<a>, len: int): vslice<a>
  v.slice(0, len)

// TODO: is there a default argument qualifier? Like arg/len?
pub fun vslice/slice(v: vslice<a>, start: int, len: int): vslice<a>
  Vslice(v.parent, max(0, min(v.length, start)), max(0, min(v.length, len)))

pub fun vslice/start/slice(v: vslice<a>, start: int): vslice<a>
  v.slice(start, v.length)

pub fun vslice/len/slice(v: vslice<a>, len: int): vslice<a>
  v.slice(0, len)

pub fun vector/lexi/cmp(xs: vector<a>, ys: vector<a>, ?cmp: (a,a) -> order): order
  xs.slice.lexi/cmp(ys.slice)

pub fun lexi/cmp(xs: vslice<a>, ys: vslice<a>, ?cmp: (a,a) -> order): order
  val x-len = xs.length
  val y-len = ys.length
  val min-len = min(x-len, y-len)
  val common-indices = range/list(min-len)
  val result = common-indices.foreach-while fn(i)
    match cmp(xs.unsafe-at(i), ys.unsafe-at(i))
      Lt -> Just(Lt)
      Gt -> Just(Gt)
      Eq -> Nothing // continue
  result.default(cmp(x-len, y-len))

pub fun vector/lexi/cmp-reverse(xs: vector<a>, ys: vector<a>, ?cmp: (a,a) -> order): order
  xs.slice.lexi/cmp-reverse(ys.slice)

pub fun lexi/cmp-reverse(xs: vslice<a>, ys: vslice<a>, ?cmp: (a,a) -> order): order
  val x-len = xs.length
  val y-len = ys.length
  val min-len = min(x-len, y-len)
  val common-indices = range/list(min-len)
  val result = common-indices.foreach-while fn(i)
    val xsi = x-len - 1 - i
    val ysi = y-len - 1 - i
    match cmp(xs.unsafe-at(xsi), ys.unsafe-at(ysi))
      Lt -> Just(Lt)
      Gt -> Just(Gt)
      Eq -> Nothing // continue
  result.default(cmp(x-len, y-len))
  

pub fun vslice/at(v: vslice<a>, i: int): maybe<a>
  if i < v.length then
    v.parent.at(v.start + i)
  else
    Nothing

pub fun vslice/unsafe-at(v: vslice<a>, i: int): a
  with unsafe-no-exn
  v.parent.at(v.start + i).unjust

pub fun vslice/is-empty(v: vslice<a>): bool
  !v.length.is-pos

pub fun vslice/next(v: vslice<a>): maybe<(a,vslice<a>)>
  match v.at(0)
    Nothing -> Nothing
    Just(a) -> Just((a, v(start_ = v.start + 1, len_ = v.length - 1)))

// A single step in an iterative computation
pub value type loop<i,s,r>
  Loop(it: i, loop-state: s)
  Stop(ret: r)

// Do a tailrecursive loop over all elements in a vslice
// A loop-state can be passed from one step to the next
pub fun vslice/loop(v: vslice<a>, loop-state: s, body: (maybe<(a,vslice<a>)>,s) -> e loop<vslice<a>,s,r>): e r
  match body(v.next, loop-state)
    Stop(ret) -> ret
    Loop(it, next-loop-state) -> loop(pretend-decreasing(it), next-loop-state, body)

pub fun vslice/foreach-indexed(v: vslice<a>, action: (int, a) -> e ()): e ()
  v.loop(0) fn(it, i)
    match it
      Nothing ->
        Stop(())
      Just((x, xs)) -> 
        action(i, x)
        Loop(xs, i + 1)

// Implementation without loop
// fun vslice/foreach( v: vslice<a>, action: a -> e () ): e ()
//   match v.next
//     Nothing -> ()
//     Just((x, xs)) -> 
//       action(x)
//       foreach(pretend-decreasing(xs), action)

pub fun vslice/foreach(v: vslice<a>, action: a -> e ()): e ()
  v.foreach-indexed fn(_, x) action(x)

pub fun vslice/vector(v: vslice<a>): vector<a>
  v.map(id)

pub fun vslice/string(v: vslice<char>): string
  v.vector.string

pub fun vslice/map(v: vslice<a>, f: a -> e b): e vector<b>
  v.map-indexed(fn(_, x) f(x))

pub fun vslice/map-indexed(v: vslice<a>, f: (int, a) -> e b): e vector<b>
  val w = unsafe-vector(v.length.ssize_t)
  v.foreach-indexed fn(i,x)
    unsafe-assign(w, i.ssize_t, f(i, x))
  w

pub fun vector/starts-with(haystack: vector<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  haystack.slice.starts-with(needle)

pub fun vslice/starts-with(haystack: vslice<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  if needle.length > haystack.length then
    False
  else
    val result = for-while(needle.length) fn(i)
      if needle.at(i) == haystack.at(i) then
        Nothing
      else
        Just(False)
    result.default(True)

pub fun vector/ends-with(haystack: vector<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  haystack.slice.ends-with(needle)

pub fun vslice/ends-with(haystack: vslice<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  if needle.length > haystack.length then
    False
  else
    val result = for-while(needle.length) fn(i)
      if needle.at(needle.length - 1 - i) == haystack.at(haystack.length - 1 - i) then
        Nothing
      else
        Just(False)
    result.default(True)

pub fun vector/contains(haystack: vector<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  haystack.slice.contains(needle)

pub fun vslice/contains(haystack: vslice<a>, needle: vslice<a>, ?(==): (a,a) -> bool, ?show: a -> string): bool
  val result = for-while(haystack.length - needle.length + 1) fn(i)
    if haystack.start/slice(i).starts-with(needle) then
      Just(True)
    else
      Nothing
  result.default(False)

