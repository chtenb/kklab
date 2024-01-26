import std/core/undiv
import std/core/unsafe

abstract value struct vslice<a>( vec : vector<a>, start : int, len : int )

pub fun vector/slice( v : vector<a> ) : vslice<a>
  Vslice(v,0,v.length)

pub fun vslice/at( slice : vslice<a>, i : int ) : maybe<a>
  slice.vec.at(slice.start + i)

pub fun vslice/is-empty( slice : vslice<a> ) : bool
  !slice.len.is-pos

pub fun vslice/next( slice : vslice<a> ) : maybe<(a,vslice<a>)>
  match slice.at(0)
    Nothing -> Nothing
    Just(a) -> Just((a, slice(start = slice.start + 1, len = slice.len - 1)))
      
fun vslice/foreach( slice : vslice<a>, action : a -> e () ) : e ()
  match slice.next
    Nothing -> ()
    Just((x, xs)) -> 
      action(x)
      foreach(pretend-decreasing(xs), action)
