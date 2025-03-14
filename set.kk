module set

import util

// Inefficient set implementation
pub struct set<a>(list : list<a>)

pub val empty : forall<a> set<a> = Set([])

pub fun append(x : set<a>, y : set<a>, ?(<=) : (a,a) -> bool) : set<a>
  (x.list ++ y.list).set

pub fun list/set(l : list<a>, ?(<=) : (a,a) -> bool) : set<a>
  Set(l.sort.uniq)

pub fun show(s : set<a>, ?show : a -> string) : string
  s.list.show
