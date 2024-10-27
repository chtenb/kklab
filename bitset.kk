pub struct bitset(value : int)

extern add(x: int32, y: int32): int32
  c inline "($1 + $2)"
  js inline "$1 + $2"

fun index(b : bitset): int
  fun rec(c : bitset, acc : int)
    if c & 1 then
      acc
    else
      rec(c >> 1, count + 1)
  rec(b, 0)
