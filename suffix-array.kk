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

type suffix-class
  S
  L

pub fun suffix-class/show(sc : suffix-class) : string
  match sc
    S -> "S"
    L -> "L"

pub fun classify-suffixes(s : vector<a>, ?cmp : (a, a) -> order, ?show : a -> string) : _ vector<suffix-class>
  val l = s.length
  val result = vector(l, L)
  var streak-start := 0
  s.foreach-indexed fn(i, t)
    when (i < l - 1)
      match cmp(t, s[i+1])
        Lt ->
          list(streak-start, i).foreach fn(j)
            result.unsafe-assign(j.ssize_t, S)
          streak-start := i + 1
        Gt ->
          // L is  the default
          streak-start := i + 1
        Eq -> ()
  result
  
