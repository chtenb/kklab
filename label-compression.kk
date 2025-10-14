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
pub alias pattern<a> = list<maybe<a>>

pub struct compression<a>(label: vector<a>, compressed: list<vslice<a>>, cost: int)

pub fun uncompressed(labels: list<vector<a>>, ?cmp: (a,a) -> order, ?show: a -> string): list<compression<a>>
  labels.map fn(label)
    Compression(label, [label.slice], label.length)

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
      compression(compressed = [compression.label.len/slice(shortest-len)], cost = new-cost)
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
      val new-compressed = [compression.label.start/slice(compression.label.length - shortest-len)]
      compression(compressed = new-compressed, cost = new-cost)
    else
      compression

// SUCi

// TODO: you can make this much faster by only comparing with prefix matches
pub fun first-shortest-uniq-circumfix(
    compression : compression<a>, labels: list<vector<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ compression<a>
  val label = compression.label
  var shortest := [label.slice]
  var shortest-len := label.length
  // We encode the circum slice as a normal slice, but then interpret it as its complement
  // Skip the first index, as we don't do postfixes so the first part must be non-empty
  for-dec(label.length - 2, 1) fn(start)
    // Skip the last index, as we don't do postfixes so the last part must be non-empty
    // Only consider len >= 2, because single width wildcards are never optimal
    for-while-dec(label.length - start - 1, 2) fn(len)
      val circum-len = label.length - len
      if circum-len >= shortest-len then
        Just(())
      else
        val pre = label.len/slice(start)
        val post = label.start/slice(start + len)
        val is-uniq = labels.foreach-while fn(other)
          // trace(other.show ++ " starts with " ++ pre.show ++ ": " ++ other.starts-with(pre).show)
          // trace(other.show ++ " ends with " ++ post.show ++ ": " ++ other.ends-with(post).show)
          if other != label && other.starts-with(pre) && other.ends-with(post) then
            Just(False)
          else
            Nothing
        if is-uniq.default(True) then
          shortest := [pre, post]
          shortest-len := circum-len
          Just(())
        else
          Nothing
    ()
  val new-cost = shortest-len + 1 // Add 1 to the cost because is 1 wildcard
  if new-cost < compression.cost then
    compression(compressed = shortest, cost = new-cost)
  else
    compression

// Pass the labels with the scores so far, for pruning reasons
pub fun compress-by-suci(
    labels: list<compression<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ list<compression<a>>
  labels.map fn(compression)
    first-shortest-uniq-circumfix(compression, labels.map(label))

// SUI

pub fun first-shortest-uniq-infix(
    compression : compression<a>, labels: list<vector<a>>, ?cmp: (a,a) -> order, ?show: a -> string
  ): _ compression<a>
  val label = compression.label
  var shortest := [label.slice]
  var shortest-len := label.length
  // Skip the first two and last two starting indices, as we don't do prefixes, and a one-length wildcard is never optimal
  trace("\nFor label " ++ label.show)
  for(2, label.length - 3) fn(start : int)
    trace("From start " ++ start.show)
    // Skip the last two ending indices, as we don't do postfixes, and a one-length wildcard is never optimal
    for-while(1, min(label.length - 2 - start, shortest-len - 1)) fn(len)
      val substr = label.slice(start, len)
      trace("Checking " ++ substr.show)
      assert("substr should not be empty", !substr.is-empty)
      val is-uniq = labels.foreach-while fn(other)
        trace("Against other " ++ other.show)
        // Contains must only check non-prefix and non-postfix substrings
        if other == label || !other.slice(1, other.length - 2).contains(substr) then
          Nothing
        else
          Just(False)
      if is-uniq.default(True) then
        shortest := [substr]
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
  var pattern := compression.compressed.map(fn(sub) sub.map(Just).list).joinsep([Nothing])
  when (compression.compressed.head.unjust.start > 0)
    pattern := [Nothing].append(pattern)
  when (compression.compressed.last.unjust.end < compression.label.length)
    pattern := pattern.append([Nothing])
  pattern
  
pub fun compress(labels: list<vector<a>>, ?cmp: (a,a) -> order, ?show: a -> string): _ list<(vector<a>, pattern<a>)>
  var results := uncompressed(labels)
  // results := compress-by-supr(results)
  // results := compress-by-supo(results)
  // results := compress-by-suci(results)
  results := compress-by-sui(results)
  results.sort-by(label).map(fn (c) (c.label, c.pattern))

