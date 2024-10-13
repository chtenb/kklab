import gen
import std/num/random
import std/num/int32

fun main()
  val seed = srandom-int32().int
  println("Generated seed " ++ seed.show)
  with pseudo-random(seed)
  // println(gen/choose([(0.1, {"x"}), (0.9, {"y"})]).().show)
  println(gen/int(0, 10).show)

