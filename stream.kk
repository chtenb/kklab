import std/core/unsafe
import std/core/undiv
import util
import iter

// A stream effect where items can be yielded one at a time
pub effect yield<a>
  ctl yield(elem : a) : ()
  
// More readable type for streaming functions
pub alias stream<a,e> = () -> <yield<a>|e> ()
  
// Effect for modelling infinite streams
// This is a specific form of divergence, where the stream is guaranteed to keep yielding values
pub type inf :: X
  
pub alias endless<a,e> = () -> <yield<a>,inf|e> ()
  
// _Unsafe_. This function removes the inf effect.
fun pretend-finite( action : () -> <inf|e> a ) : e a
  pretend-finite-cast( action )()

inline fip extern pretend-finite-cast : forall<e,a> ( action : () -> <inf|e> a ) -> (() -> e a)
  inline "#1"
  
// Convert an inf effect to a div effect
pub fun diverge( action : () -> <inf|e> a ) : <div|e> a
  mask<div>
    pretend-finite(action)


/// Constructors

pub fun empty() : yield<a> ()
  ()
  
pub fun single(value : a) : yield<a> ()
  yield(value)


pub fun repeat(value : a) : <yield<a>,inf> ()
  yield(value)
  repeat(pretend-decreasing(value))

pub fun inf/take(it : endless<a,e>, n : int) : <yield<a>|e> ()
  fin/take(fn() pretend-finite(it), n)

pub fun fin/take(it : stream<a,e>, n : int) : <yield<a>|e> ()
  var count := 0
  with override ctl yield(elem)
    if count < n then
      yield(elem) // Forward the yield to our new yield function
      count := count + 1
      resume(())
    else
      ()
  it()

pub fun drop(it : stream<a,e>, n : int) : <yield<a>|e> ()
  var count := 0
  with override ctl yield(elem)
    if count < n then
      // Ignore this element
      count := count + 1
      resume(())
    else
      // After n elements, simply return the continuation
      yield(elem)
      resume(())
  it()

pub fun map(it : stream<a,e>, f : a -> b) : <yield<b>|e> ()
  it.iter.map(f).iter/stream

// pub fun map(it : stream<a,e>, f : a -> b) : <yield<b>|e> ()
//   with override ctl yield(elem)
//     yield(f(elem))
//     resume(())
//   it()

pub fun list/stream(l : list<a>) : yield<a> ()
  l.foreach(yield)
  
pub fun vector/stream(l : vector<a>) : yield<a> ()
  l.foreach(yield)
        
pub fun iter/stream(it : iter<a,e>) : <yield<a>|e> ()
  val n = 
    mask<yield>
      it.next
  match n
    Nothing -> ()
    Just((a, rest)) ->
      yield(a)
      iter/stream(pretend-decreasing(rest))()
  
pub fun append(first : stream<a,e>, second : stream<a,e>) : <yield<a>|e> ()
  first()
  second()
  
pub fun concat(streams : stream<stream<a,e>,total>) : <yield<a>|e> ()
  streams.foreach fn(it)
    it()

pub fun stride/range(lo : int, hi : int, stride : int) : yield<int> ()
  if hi < lo then
    ()
  else
    yield(lo)
    stride/range(pretend-decreasing(lo + stride),hi,stride)
    
pub fun range(lo : int, hi : int) : yield<int> ()
  stride/range(lo,hi,1)

pub fun filter(it : stream<a,e>, pred : a -> e bool) : <yield<a>|e> ()
  with override ctl yield(e)
    when(mask<yield>({pred(e)}))
      yield(e)
    resume(())
  it()


/// Consumers

fun foldl(it : stream<a,e>, empty : b, f : (b, a) -> e b) : e b
  it.iter.foldl(empty, f)
// fun show( xs : stream<a,e>, ?show : a -> e string ) : e string
  
fun default/head( it : stream<a,e>, default : a ) : e a
  handle it
    return (_) default
    ctl yield(e)
      e
  
pub fun contains(it : stream<a,e>, elem : a, ?(==) : (a,a) -> bool) : e bool
  handle it
    return (_) False
    ctl yield(e)
      if e == elem then
        True
      else
        resume(())
    
fun all(it : stream<a,e>, pred : a -> e bool ) : e bool
  handle it
    return (_) True
    ctl yield(e)
      if !pred(e) then
        False
      else
        resume(())
        
fun any(it : stream<a,e>, pred : a -> e bool ) : e bool
  handle it
    return (_) False
    ctl yield(e)
      if pred(e) then
        True
      else
        resume(())

pub fun foreach(it : stream<a,e>, do : a -> e ()) : e ()
  handle it
    ctl yield(elem)
      do(elem) // Run the foreach block for each element
      resume(()) // We don't want just the first element, so return to call site
      
pub fun find-maybe(it : stream<a,e>, do : a -> e maybe<b>) : e maybe<b>
  handle it
    return (_) Nothing
    ctl yield(elem)
      match do(elem)
        Nothing -> resume(())
        Just(result) -> Just(result)

// Returns the index of the first element where `pred` holds, or `-1` if no such element exists.
pub fun index-of(it : stream<a,e>, pred : a -> e bool) : e int
  it.iter.index-of(pred)
// pub fun index-of(it : stream<a,e>, pred : a -> e bool) : e int
//   var index := -1
//   handle it
//     return (_) -1
//     ctl yield(a)
//       index := index + 1
//       if pred(a) then index else resume(())


/// Converters

pub fun iter(s : stream<a,e>) : iter<a,e>
  Iter
    handle s
      return(_) Nothing
      ctl yield(a)
        Just((a, Iter(fn() resume(()))))

pub fun list(it : stream<a,e>) : e list<a>
  handle it
    return (_) Nil
    ctl yield(elem)
      Cons(elem, resume(()))

// pub fun zip(streams : list<stream<a,e>>) : yield<list<a>|e> ()
//   val sources //: some<h,a,e> vector<ref<h, iter<a,e>>> 
//     = streams.map(fn(s) s.iter.ref).vector
//   val n = sources.length
//   zip-rec()
  
// fun zip-rec() //: _ stream<list<a>,e>
//   result := Nil
//   finished := False
//   list(0, n - 1).foreach fn(i)
//     val current = mask<local>{!sources.unsafe-at(i)}
//     val nxt = mask<local>{ current.next }
//     match nxt
//       Nothing -> 
//         finished := True
//       Just((x, rest)) ->
//         result := Cons(x, result)
//         val r = sources.unsafe-at(i)
//         r := rest
//         ()
//   if finished then
//     ()
//   else
//     yield(result)
//     zip-rec()

// pub fun forkzip(g : stream<a,e>, fs : list<stream<a,e>, stream<b,e>>) : stream<list<b>,e>
//   fn()
//     val n = fs.length
//     val functions = fs.vector
//     val buffers : vector<ref<h,queue<a>>> = functions.map(fn(_) queue/empty())
//     var iteration : ref<h, iter<a,e>> = g.iter
//     var iters : vector<ref<h,iter<a,e>>> = functions.map-index fn(_,i) 
//       Iter
//         match buffers.at(i).pop()
//           Just((x,q)) -> 
//             buffers[i] := q
//             Just(x)
//           Nothing -> 
//             match iteration.next
//               Just(x, it) ->
//                 list(n).foreach fn(j)
//                   when(i != j) 
//                     buffers[i].push(x)
//                 iteration := it
//                 Just(x)
//               Nothing ->
//                 Nothing
//     val inputs = iters.map(stream)
//     val outputs = functions.map-index(fn(f,i) f(inputs[i]))
//     outputs.list.zip()
