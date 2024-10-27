
struct car(length : int, width : int, height : int)

fun show(car : car) : string
  // match car
  //   CarFormat(l,w,h) -> "hallo"
  "formaat: " ++ car.length.show ++ "x" ++ car.width.show ++ "x" ++ car.height.show

fun volume(car : car) : int
  car.length * car.width * car.height

struct furniture(length : int, width : int, height : int)

struct point(x : int, y : int, z : int)
struct corners(points : list<point>)

fun point/fits-inside(point : point, car : car) : bool
  point.x <= car.length
    && point.y <= car.width
    && point.z <= car.height
  
fun corner/fits-inside(corners : corners, car : car) : bool
  corners.points.all fn(p) p.fits-inside(car)
  
/*
      ____
     /   /|
    /   / |
   /___/  /
   |   | /O
   |   |/O
   |---| 

   *
  / \
 *   *
/ \
* *
  
*/

type intlist
  Empty
  Element(i : int, rest : intlist)

type tree
  Leaf(i : int)
  Node(left : tree, right : tree) 
  
effect state<a>
  fun get() : a
  fun set( x : a ) : ()

fun sumdown( sum : int = 0 ) : <state<int>,div> int
  val i = get()
  if i <= 0 then sum else
    set( i - 1 )
    sumdown( sum + i )

fun state( init : a, action : () -> <state<a>,div|e> b ) : <div|e> b
  var st := init
  with handler
    fun get()  st
    fun set(i) 
      if True then
        st := i
      else ()
  action()
