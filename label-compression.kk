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
import stream
import vslice

pub fun cmp-lr(xs : vector<a>, ys : vector<a>, ?cmp : (a,a) -> order): order
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


pub alias pattern<a> = vector<maybe<a>>
pub alias compressed-vector<a> = (vector<a>, pattern<a>)

pub fun minimal-uniq-prefix-length(label : vector<a>, neighbors : list<vector<a>>, ?(==) : (a,a) -> bool, ?show : a -> string) : _ int
  val diff-indices = neighbors.map fn(neighbor)
    range/list(0, label.length - 1).index-of fn(i)
      neighbor[i] != label[i]
  diff-indices.maximum + 1

pub fun minimal-uniq-prefix(label : vector<a>, neighbors : list<vector<a>>, ?(==) : (a,a) -> bool, ?show : a -> string) : _ pattern<a>
  val l = minimal-uniq-prefix-length(label, neighbors)
  label.map-indexed(fn(i, c) if i < l then Just(c) else Nothing)

pub fun minimal-uniq-substr(label : vector<a>, ?cmp : (a,a) -> order, ?show : a -> string) : _ compressed-vector<a>
  todo()

pub fun compress(labels : list<vector<a>>, ?cmp : (a,a) -> order, ?show : a -> string) : _ list<compressed-vector<a>>
  val here/cmp = cmp-lr
  val sorted-lr = labels.sort
  val sorted-lr-vec = sorted-lr.vector
  val label-count = labels.length
  val patterns = sorted-lr.map-indexed fn(i, l)
    val neighbors = list
      when (i > 0)
        yield(sorted-lr-vec[i - 1])
      when (i < label-count - 1)
        yield(sorted-lr-vec[i + 1])
    minimal-uniq-prefix(l, neighbors)
  sorted-lr.zip(patterns)


  
