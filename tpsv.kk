import std/core/undiv
import std/core/unsafe
import std/core/debug
import std/core/list

import list
import string

// A table is a list of rows
pub alias table = list<list<string>>

fun read-tsv(input: string): table
  input.split("\n").map(fn(line) line.split("\t"))
  
    
fun read-markdown(input: string): table
  val lines = input.split("\n").remove-at(1)
  lines.map(fn(line) line.substring(1, line.count - 2).split("|").map(trim))

fun write-tpsv(t: table): string
  val lines = t.map fn(row)
    row.list/joinsep/join("\t|")
  lines.list/joinsep/join("\n")
   


