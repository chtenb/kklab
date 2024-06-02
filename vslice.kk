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

pub value type tailrec<i,o,r>
  Loop(it : i, loop-arg : o)
  Stop(ret : r)

pub fun vslice/loop( slice : vslice<a>, f : (maybe<(a,vslice<a>)>, o, b) -> e tailrec<vslice<a>,o,r>, loop-arg : o, fixed-arg : b ) : e r
  match f(slice.next, loop-arg, fixed-arg)
    Stop(ret) -> ret
    Loop(it) -> loop(pretend-decreasing(it), f, loop-arg, fixed-arg)
      
fun vslice/foreach( slice : vslice<a>, action : a -> e () ) : e ()
  loop(slice, foreach-rec, (), action)

fun vslice/foreach-rec( it : maybe<(a,vslice<a>)>, _ : (), action : a -> e () ) : e tailrec<vslice<a>,(),()>
  match it
    Nothing -> Stop(())
    Just((x, xs)) -> 
      action(x)
      Loop(xs,())

// fun vslice/foreach( slice : vslice<a>, action : a -> e () ) : e ()
//   match slice.next
//     Nothing -> ()
//     Just((x, xs)) -> 
//       action(x)
//       foreach(pretend-decreasing(xs), action)
