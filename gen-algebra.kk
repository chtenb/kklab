// This gen implementation follows the algebra-interpreter pattern.
// Instead of executing effects directly, we build a syntax tree representing the sample space. Then afterwards we sample this space.
// The benefit of this approach is that it is very simple to follow and to implement.


// TODO: can we use an effect akin each to have list comprehensions?
// This could be used to implement a more convenient DSL for weighing and flattening

import util
import list
import std/core/undiv
import std/core/unsafe
import std/num/random
import std/num/int32
import std/num/float64
import std/num/ddouble
import std/core/debug
import std/core/list
import rng
import each
import stream
import std/time
import std/time/duration

abstract type gen<a>
  Outcome(value : a)
  Space(get-gens : () -> <pure,random> list<gen<a>>)
  
// All the subspaces in a Space have equal probability.
// This means there is an isomorphism from any Space to a Space with no subspaces.

pub fun sample(g : gen<a>) : <pure,random> a
  match g
    Outcome(value) -> value
    Space(get-gens) ->
      val gens = get-gens()
      val count = gens.length
      if count == 0 then
        throw("Empty sample space") // Backtrack
      else
        val i = uniform/int(count)
        val choice = unsafe-no-exn
          gens.at(i).unjust
        match (try { sample(choice) })
          Ok(value) -> value
          Error(_) -> sample(Space { gens.remove-at(i) })
  
// 
// Algebraic constructors
// 

fun pure(value : a) : gen<a>
  Outcome(value)

fun impossible() : gen<a>
  Space(fn() { [] })

fun random/gen(r : () -> <pure,random> a) : gen<a>
  Space
    [ Outcome(r())
    ]

fun map(g : gen<a>, f : a -> b) : gen<b>
  match g
    Outcome(value) -> Outcome(f(value))
    Space(s) -> Space
      val gens = s()
      gens.map fn(gen)
        pretend-decreasing(gen).map'(f)

fun map'(g : gen<a>, f : a -> b) : gen<b>
  g.bind(f >. pure)
  
fun apply(g : gen<a>, f : gen<a -> b>) : gen<b>
  Space
    val a = sample(g)
    val f' = sample(f)
    [ Outcome(f'(a))
    ]
    
fun apply'(g : gen<a>, f : gen<a -> b>) : gen<b>
  gen
    sample(f)(sample(g))

fun bind(space : gen<a>, f : a -> gen<b>) : gen<b>
  Space
    val a = sample(space)
    [ f(a)
    ]
    
fun bind'(space : gen<a>, f : a -> gen<b>) : gen<b>
  Space
    [ f(sample(space))
    ]

//
// Sample space builder effect
// 

fun yield/gen(get-gens : () -> <pure,random,yield<gen<a>>> ()) : gen<a>
  Space
    list(get-gens)

fun uniform/space(get-gens : () -> <pure,random,yield<gen<a>>> ()) : <pure,random,yield<gen<a>>> ()
  yield(
    Space
      list(get-gens)
  )

fun weighted/space(weight : int, get-gens : () -> <pure,random,yield<gen<a>>> ()) : <pure,random,yield<gen<a>>> ()
  repeat(weight)
    space(get-gens)

fun outcome(value : a) : yield<gen<a>> ()
  yield(Outcome(value))

fun yield-from(gen : gen<a>) : <pure,random,yield<gen<a>>> ()
  match gen
    Outcome(value) -> outcome(value)
    Space(get-gens) -> get-gens().foreach fn(g)
      yield(g)

// 
// Misc converters
// 

fun maybe/gen(a : maybe<a>) : gen<a>
  match a
    Nothing -> impossible()
    Just(value) -> Outcome(value)

// 
// Common gens
// 
  
// Generate True with probability k/n
fun k/n/bool(k: int, n: int) : gen<bool>
  gen
    uniform/int(0, n) < k
  
// Generate True with probability 1/n
fun n/bool(n: int) : gen<bool>
  bool(1, n)
  
// Generate True with probability 1/2
val bool : gen<bool> =
  bool(1, 2)

fun int(lower: int, upper: int) : gen<int>
  gen
    space
      [-1, 0, 1, lower, lower + 1, upper - 1, upper - 2].foreach fn(i)
        when(lower <= i && i <= upper)
          outcome(i)
    space(2)
      outcome(uniform/int(lower, upper))

fun positive/int(upper: int) : gen<int>
  int(0, upper)

fun fixed-list(length: int, gen-elem: gen<a>): gen<list<a>>
  gen
    list(1, length).map fn(_)
      sample(gen-elem)

fun list(max-length: int, gen-elem: gen<a>): gen<list<a>>
  with length <- int(max-length + 1).bind
  fixed-list(length, gen-elem)

fun fixed-string(length: int, gen-char: gen<char>): gen<string>
  fixed-list(length, gen-char).map(listchar/string)

fun string(max-length: int, gen-char: gen<char>): gen<string>
  list(max-length, gen-char).map(listchar/string)

fun range/char(min: char, max: char) : gen<char>
  gen
    uniform/int(min.int, max.int).char
    
val lowercase : gen<char> = char('a', 'z')
val uppercase : gen<char> = char('A', 'Z')
val digit : gen<char> = char('0', '9')
val alphanum : gen<char> = 
  gen
    yield-from(lowercase)
    yield-from(uppercase)
    yield-from(digit)
    outcome('_')


fun float64() : gen<float64>
  todo()
  
fun ddouble() : gen<ddouble>
  with secs <- float64().bind
  with frac <- float64().map
  ddouble/ddouble(secs, frac)
  

fun main()
  val seed = srandom-int32().int
  println("Generated seed " ++ seed.show)
  with pseudo-random(seed)
  println(sample(list(5, int(10))).show)
