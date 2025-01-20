import std/core/undiv

pub struct bitset(value : int)

fun show-binary(i : int, padding : int = 0) : string
  assert("only positive", i >= 0)
  val sep = if padding % 8 == 0 then ":" else ""
  if i == 0 then 
    if padding > 0 then show-binary(pretend-decreasing(i), pretend-decreasing(padding - 1)) ++ "0" ++ sep else ""
  else 
    show-binary(pretend-decreasing(divmod(i, 2).fst), pretend-decreasing(padding - 1)) ++ (if is-even(i) then "0" else "1") ++ sep
  
// extern import
//   c  file "int.h"
  
// inline fip extern int-and : (int,int) -> int
//   c  "kk_integer_and"

// fun index(b : bitset): int
//   fun rec(c : bitset, acc : int)
//     if c & 1 then
//       acc
//     else
//       rec(c >> 1, count + 1)
//   rec(b, 0)
fun main()
  // When one operand exceeds 32 bits the answer is wrong
  val x = 9999999999
  val y = 999999999
  val z = x & y
  println(x.show-binary(64))
  println(y.show-binary(64))
  println(z.show-binary(64))
  ()
