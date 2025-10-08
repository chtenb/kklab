import std/num/random
import std/num/int32
import std/core/char

import vector
import vslice

// import tpsv
// fun main()
//   val tsv = "a\tb\tc\t\n1\t2\t3"
//   val markdown = "|a|b|c|\n|-|-|-|\n|1|2|3|"
//   read-tsv(tsv).println
//   read-markdown(markdown).write-tpsv(8).println


import label-compression
fun main()
  println("abcaa".vector.slice.starts-with("ab".vector.slice).show)
  println("abcaa".vector.slice.contains("ab".vector.slice).show)
  println("abcaa".vector.slice.contains("bc".vector.slice).show)
  println("abcaa".vector.slice.contains("ca".vector.slice).show)
  println("abcaa".vector.slice.contains("aa".vector.slice).show)
  println("abcaa".vector.slice.contains("abc".vector.slice).show)
  println("abcaa".vector.slice.contains("bca".vector.slice).show)
  println("abcaa".vector.slice.contains("caa".vector.slice).show)
  println("abcaa".vector.slice.contains("abcaa".vector.slice).show)
  println(compress(["abcaa".vector, "abdbb".vector]).show)


// import suffix-array
// fun main()
//   println(classify-suffixes("mississippi".vector))


