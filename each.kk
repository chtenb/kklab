
pub effect each
  ctl each(elems : list<a>) : a
  ctl fail() : ()

pub fun guard(pred : bool) : each ()
  if pred then () else fail()

pub fun list-comp(action : () -> each a) : list<a>
  with handler
    return (x) [x]
    ctl each(elems) elems.flatmap(resume)
    ctl fail() []
  action()

pub fun main()
  val l = list-comp
    val x = each(list(1,1000))
    val y = each(list(1,1000))
    val z = x+y
    guard (z != 3)
    z * z
  l.sum()

