
pub effect each
  ctl each(elems : list<a>) : a
  ctl fail() : a

pub fun guard(pred : bool) : each ()
  if pred then () else fail()

pub fun list-comp(action : () -> each a) : list<a>
  with handler
    return (x) [x]
    ctl each(elems) elems.flatmap(resume)
    ctl fail() []
  action()

pub fun test-each()
  list-comp
    val x = each([1,2,3])
    val y = each([1,2,2])
    val z = x+y
    guard (z != 3)
    z * z

