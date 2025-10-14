import std/num/random
import std/num/int32
import std/core/char
import std/os/path
import std/os/file
import std/num/float64

import vector
import vslice
import util
import list
import string

// import tpsv
// fun main()
//   val tsv = "a\tb\tc\t\n1\t2\t3"
//   val markdown = "|a|b|c|\n|-|-|-|\n|1|2|3|"
//   read-tsv(tsv).println
//   read-markdown(markdown).write-tpsv(8).println


import label-compression
fun parse-labels() : io list<vector<char>>
  val fpath = "C:\\Users\\ChieltenBrinke\\Desktop\\Storage Group for all Volumes by Volume Label.tsv"
  val tsv = read-text-file(fpath.path)
  tsv.lines.drop(1).map(fn(line) line.split("\t")).flatmap-maybe(fn(l)
    match l
      Cons(stgrp, Cons(volser, Nil)) -> Just(volser.vector)
      _ ->  Nothing
  ).sort.dedup
fun pattern/show(pat : list<maybe<char>>) : string
  pat.map(fn(x)
    match x
      Nothing -> ".."
      Just(c) -> c.string
  ).join
fun main(): _ ()
  // val labels = ["abcaa", "abdbb"].map(fn(s) s.vector)
  val labels = parse-labels() //.filter-indexed(fn(i, _) i % 10 == 0)
  println("Label count: " ++ labels.length.show)
  var label-length := 0
  var pattern-length := 0
  compress(labels).foreach fn((label, pattern))
    println(label.vector/string ++ "  ->  " ++ pattern.pattern/show)
    label-length := label-length + label.length
    pattern-length := pattern-length + pattern.length
  println("Label count: " ++ labels.length.show)
  println("Total length: " ++ label-length.show)
  println("Compressed length: " ++ pattern-length.show)
  println("Compression ratio: " ++ (label-length.float64 / pattern-length.float64).show)


// import suffix-array
// fun main()
//   println(classify-suffixes("mississippi".vector))


