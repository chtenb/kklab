import std/num/random
import std/num/int32
import std/core/char
import std/os/path
import std/os/file

import vector
import vslice
import util
import list

// import tpsv
// fun main()
//   val tsv = "a\tb\tc\t\n1\t2\t3"
//   val markdown = "|a|b|c|\n|-|-|-|\n|1|2|3|"
//   read-tsv(tsv).println
//   read-markdown(markdown).write-tpsv(8).println


import label-compression
fun parse-storage-groups() : io list<vector<char>>
  val fpath = "C:\\Users\\ChieltenBrinke\\Desktop\\Storage Group for all Volumes by Volume Label.tsv"
  val tsv = read-text-file(fpath.path)
  tsv.lines.drop(1).map(fn(line) line.split("\t")).flatmap-maybe(fn(l)
    match l
      Cons(stgrp, Cons(_, Nil)) -> Just(stgrp.vector)
      _ -> 
        // println("can't parse " ++ l.show)
        Nothing
  ).sort.dedup
fun pattern/show(pat : vector<maybe<char>>) : string
  pat.map(fn(x)
    match x
      Nothing -> ".."
      Just(c) -> c.string
  ).vector/join
fun main(): _ ()
  // val labels = ["abcaa", "abdbb"].map(fn(s) s.vector)
  val labels = parse-storage-groups()
  compress(labels).foreach fn((label, pattern))
    println(label.vector/string ++ "  ->  " ++ pattern.pattern/show)
  println("Label count: " ++ labels.length.show)


// import suffix-array
// fun main()
//   println(classify-suffixes("mississippi".vector))


