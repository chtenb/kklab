import std/core/undiv
import std/core/unsafe
import std/core/debug
import std/core/list
import std/core/vector

import coka-std/std/core-extras

import util
import list
import string
import vector

pub alias label<a> = vector<a>

pub fun cmp-lr(xs : label<a>, ys : label<a>, ?cmp : (a,a) -> order): order
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

pub alias pattern<a> = label<maybe<a>>
pub alias compressed-label<a> = (label<a>, pattern<a>)

pub fun minimal-uniq-prefix-length(label : label<a>, neighbors : list<label<a>>, ?(==) : (a,a) -> bool) : _ int
  val diff-indices = neighbors.map fn(neighbor)
    range/list(0, label.length - 1).index-of(fn(i) !(neighbor[i] == label[i]))
  diff-indices.maximum + 1

pub fun minimal-uniq-prefix(label : label<a>, neighbors : list<label<a>>, ?(==) : (a,a) -> bool) : _ pattern<a>
  val l = minimal-uniq-prefix-length(label, neighbors)
  label.map-indexed(fn(i, c) if i < l then Just(c) else Nothing)

pub fun compress(labels : list<label<a>>, ?cmp : (a,a) -> order) : list<compressed-label<a>>
  val here/cmp = cmp-lr
  val sorted-rl = labels.sort()
  var minimal-uniq-prefixes := []
  []
  
