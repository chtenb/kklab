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

pub struct compression<a>(label: vector<a>, compressed: vslice<a>, cost: int)

pub fun uncompressed(labels: list<vector<a>>): list<compression<a>>
  labels.map fn(label)
    Compression(label, label.slice, label.length)

// SUPR

pub fun shortest-uniq-prefix-length(
    label: vector<a>, neighbors: list<vector<a>>, ?(==): (a,a) -> bool, ?show: a -> string
  ): _ int
  val diff-indices = neighbors.map fn(neighbor)
    range/list(0, label.length - 1).index-of fn(i)
      neighbor.at(i) != label.at(i)
  if diff-indices.minimum >= 0 then
    diff-indices.maximum + 1
  else
    label.length // label is fully contained in other label

pub fun compress-by-supr(
    labels: list<compression<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ list<compression<a>>
  val sorted = labels.sort-by(label)
  val sorted-vec = sorted.vector
  val label-count = labels.length
  sorted.map-indexed fn(i, compression)
    val neighbors = list
      when (i > 0)
        yield(sorted-vec[i - 1].label)
      when (i < label-count - 1)
        yield(sorted-vec[i + 1].label)
    val shortest-len = shortest-uniq-prefix-length(compression.label, neighbors)
    val new-cost = shortest-len + 1 // Add one for wildcard
    if new-cost < compression.cost then
      compression(compressed = compression.label.len/slice(shortest-len), cost = new-cost)
    else
      compression

// SUPO

pub fun shortest-uniq-postfix-length(
    label: vector<a>, neighbors: list<vector<a>>, ?(==): (a,a) -> bool, ?show: a -> string
  ): _ int
  val diff-indices = neighbors.map fn(neighbor)
    range/list(0, label.length - 1).index-of fn(i)
      neighbor.at(neighbor.length - 1 - i) != label.at(label.length - 1 - i)
  if diff-indices.minimum >= 0 then
    diff-indices.maximum + 1
  else
    label.length // label is fully contained in other label

pub fun compress-by-supo(
    labels: list<compression<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ list<compression<a>>
  val x/cmp = vector/lexi/cmp-reverse
  val sorted = labels.sort-by(label)
  val sorted-vec = sorted.vector
  val label-count = labels.length
  sorted.map-indexed fn(i, compression)
    val neighbors = list
      when (i > 0)
        yield(sorted-vec[i - 1].label)
      when (i < label-count - 1)
        yield(sorted-vec[i + 1].label)
    val shortest-len = shortest-uniq-postfix-length(compression.label, neighbors)
    val new-cost = shortest-len + 1 // Add one for wildcard
    if new-cost < compression.cost then
      val new-compressed = compression.label.start/slice(compression.label.length - shortest-len)
      compression(compressed = new-compressed, cost = new-cost)
    else
      compression

// SUI

// TODO: exclude pre and post fixes as small optimization
pub fun first-shortest-uniq-infix(
    compression : compression<a>, labels: list<vector<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ compression<a>
  val label = compression.label
  var shortest := label.slice
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
  val new-cost = shortest-len + 2 // Add two to the cost because there are two wildcards
  if new-cost < compression.cost then
    compression(compressed = shortest, cost = new-cost)
  else
    compression

// Pass the labels with the scores so far, for pruning reasons
pub fun compress-by-sui(
    labels: list<compression<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ list<compression<a>>
  labels.map fn(compression)
    first-shortest-uniq-infix(compression, labels.map(label))


fun vslice/pattern(compression: compression<a>): _ pattern<a>
  var pattern := compression.compressed.map(Just)
  when (compression.compressed.start > 0)
    pattern := [Nothing].vector.append(pattern)
  when (compression.compressed.end < compression.label.length)
    pattern := pattern.append([Nothing].vector)
  pattern
  
pub fun compress(labels: list<vector<a>>, ?cmp: (a,a) -> order, ?show: a -> string): _ list<(vector<a>, pattern<a>)>
  var results := uncompressed(labels)
  results := compress-by-supo(results)
  results := compress-by-supr(results)
  results := compress-by-sui(results)
  results.map(fn (c) (c.label, c.pattern))

