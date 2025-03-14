struct person
  age : int
  name : string
  lastname : string
  realname : string = name

val brian = Person( 29, "Brian", "Johnson" )
  
fun birthday( p : person ) : person
  p( age = p.age + 1 )


struct transaction
  from-account : string
  to-account : string
  
val tran = Transaction("a", "b")
