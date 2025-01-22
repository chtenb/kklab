// This gen implementation follows the algebra-interpreter pattern.
// Instead of executing effects directly, we build a syntax tree representing the sample space. Then afterwards we sample this space.
// 
import util
import list
import std/core/undiv
import std/core/unsafe
import std/num/random
import std/num/int32
import std/num/float64
import std/core/debug
import rng

// pub alias gen<a> = () -> outcome<a>

pub type gen<a>
  Outcome(value : a)
  Space(s : () -> <div,random> list<gen<a>>)
  
pub fun sample(space : gen<a>) : <div,random> a
  match space
    Outcome(value) -> value
    Space(s) ->
      val subspaces = s()
      val i = uni/int(subspaces.length)
      val choice = unsafe-no-exn
        subspaces.at(i).unjust
      sample(choice)
  
fun delay(r : () -> <random,div> a) : gen<a>
  Space
    [ Outcome(r())
    ]

fun bind(space : gen<a>, f : a -> gen<b>) : gen<b>
  Space
    val a = sample(space)
    [f(a)]
  
// Generate True with probability k/n
fun k/n/gen/bool(k: int, n: int) : gen<bool>
  delay
    uni/int(0, n) < k

fun gen/int(lower: int, upper: int) : gen<int>
  Space
    [ Space
      [-1, 0, 1, lower, lower + 1, upper - 1, upper - 2]
        .filter fn(i)
          lower <= i && i <= upper
        .map fn(i)
          Outcome(i)
    , Outcome(uni/int(lower, upper))
    ]

fun positive/gen/int(upper: int) : gen<int>
  gen/int(0, upper)

fun fixed/gen/list(length: int, gen-elem: gen<a>): gen<list<a>>
  fun rec(n)
    if n <= 0 then
      []
    else
      Cons(sample(gen-elem), rec(pretend-decreasing(n - 1)))
  Space
    [ Outcome(rec(length))
    ]

fun bounded/gen/list(max-length: int, gen-elem: gen<a>): gen<list<a>>
  Space
    val size = sample(gen/int(max-length))
    [ fixed/gen/list(size, gen-elem)
    ]
    
fun main()
  val seed = srandom-int32().int
  println("Generated seed " ++ seed.show)
  with pseudo-random(seed)
  println(sample(bounded/gen/list(1, gen/int(10))).show)
