import std/core/undiv

abstract value struct vslice<a>( vec : vector<a>, start : int, len : int )

pub fun vector/slice( v : vector<a> ) : vslice<a>
  Vslice(v,0,v.length)

pub fun vslice/at( slice : vslice<a>, i : int ) : maybe<a>
  slice.vec.at(slice.start + i)

pub fun vslice/next( slice : vslice<a> ) : maybe<(a,vslice<a>)>
  match slice.at(0)
    Nothing -> Nothing
    // Just(a) -> Just((a, slice(start = slice.start + 1, len = slice.len - 1)))
    Just(a) -> Just((a, pretend-decreasing(slice(start = slice.start + 1, len = slice.len - 1))))
  
fun vslice/foreach( slice : vslice<a>, action : a -> e () ) : e ()
  match slice.next
    Nothing -> ()
    Just((x, xs)) -> 
      action(x)
      foreach(xs, action)
      // foreach(pretend-decreasing(xs), action)

  // vector-init-total(first.length + second.length, fn(i) unsafe-total({index-two(first, second, i)}))
// pub fun vector/append(first : vector<a>, second : vector<a>) : _ vector<a>
//   vector-init(first.length + second.length, fn(i) index-two(first, second, i))
  
// fun vector/index-two(first : vector<a>, second : vector<a>, i: int) : _ a
//   if i < first.length then first[i] else second[i - first.length]

// pub fun main()
//   val x = [1,2,3].vector
//   val y = x.append(x)
//   ()
