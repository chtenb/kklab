import std/num/random
import std/num/int32

import tpsv

fun main()
  val tsv = "a\tb\tc\t\n1\t2\t3"
  val markdown = "|a|b|c|\n|-|-|-|\n|1|2|3|"
  read-tsv(tsv).println
  read-markdown(markdown).println


