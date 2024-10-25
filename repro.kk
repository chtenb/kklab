
pub effect yield<a>
  ctl yield(elem : a) : ()
  
pub fun map(it : () -> <yield<a>> (), f : a -> b) : <yield<b>> ()
  with override ctl yield(elem)
    yield(f(elem))
    resume(())
  it()
  
  
