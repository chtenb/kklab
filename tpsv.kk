import std/core/undiv
import std/core/unsafe
import std/core/debug
import std/core/list
import std/core/vector

import list
import string
import vector

// A table is a list of rows
pub alias table = list<list<string>>

fun read-tsv(input: string): table
  input.split("\n").map(fn(line) line.split("\t"))
  
    
fun read-markdown(input: string): table
  val lines = input.split("\n").remove-at(1)
  lines.map(fn(line) line.substring(1, line.count - 2).split("|").map(trim))



pub fun ref/show( r : ref<h,a>, ?show : a -> e string ) : <div|e> string
  "ref(" ++ r.show ++ ")"

fun write-tpsv(t: table, tab-size: int): _ string
  match t
    Nil -> ""
    Cons(header, _) ->
      val column-widths = vector(header.list/length, ref(0))
      // t.foreach fn(row)
      //   row.foreach-indexed fn(index, cell)
      //     column-widths.unsafe-at(index).modify(fn(w) w := max(w, cell.count))
      // "hello"
      column-widths.vector/show
      // val lines = t.map fn(row)
      //   row.list/joinsep/join("\t|")
      // lines.list/joinsep/join("\n")
   


