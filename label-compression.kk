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

// Nothing indicates a multilength wildcard, there should be at most one consecutive wildcard
pub alias pattern<a> = vector<maybe<a>>

pub fun shortest-uniq-prefix-length(
    label: vslice<a>, neighbors: list<vslice<a>>, ?(==): (a,a) -> bool, ?show: a -> string
  ): _ int
  val diff-indices = neighbors.map fn(neighbor)
    range/list(0, label.length - 1).index-of fn(i)
      neighbor.at(i) != label.at(i)
  diff-indices.maximum + 1

pub fun compute-supr(
    labels: list<vslice<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ list<(vslice<a>, int)>
  val sorted = labels.sort
  val sorted-vec = sorted.vector
  val label-count = labels.length
  sorted.map-indexed fn(i, label)
    val neighbors = list
      when (i > 0)
        yield(sorted-vec[i - 1])
      when (i < label-count - 1)
        yield(sorted-vec[i + 1])
    val shortest-len = shortest-uniq-prefix-length(label, neighbors)
    (label.len/slice(shortest-len), shortest-len)


pub fun first-shortest-uniq-infix(
    label: vslice<a>, best-result: vslice<a>, best-score: int,
    labels: list<vslice<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ (vslice<a>, int)
  var shortest := label
  var shortest-len := label.length
  for(label.length - 2) fn(start)
    for-while(1, label.length - start) fn(len)
      if len >= shortest-len then
        Just(())
      else
        val substr = label.slice(start, len)
        val is-uniq = labels.foreach-while fn(other)
          if other == label || !other.contains(substr) then
            Nothing
          else
            Just(False)
        if is-uniq.default(True) then
          shortest := substr
          shortest-len := len
          Just(())
        else
          Nothing
    ()
  if shortest-len < best-score then
    (shortest, shortest-len)
  else
    (best-result, best-score)

// Pass the labels with the scores so far, for pruning reasons
pub fun compute-sui(
    labels : list<vslice<a>>, results: list<(vslice<a>, int)>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ list<(vslice<a>, int)>
  labels.zipwith(results) fn(label, best)
    val (best-result, best-score) = best
    first-shortest-uniq-infix(label, best-result, best-score, labels)

fun vslice/pattern(result: vslice<a>): _ pattern<a>
  val label = result.parent
  var pattern := result.map(Just)
  when (result.start > 0)
    pattern := [Nothing].vector.append(pattern)
  when (result.end < label.length)
    pattern := pattern.append([Nothing].vector)
  pattern
  
pub fun compress(labels: list<vslice<a>>, ?cmp: (a,a) -> order, ?show: a -> string): _ list<pattern<a>>
  var results := compute-supr(labels)
  results := compute-sui(labels, results)
  results.map(fst >. pattern)
