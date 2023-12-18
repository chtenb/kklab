
pub alias parser<e,a> = () -> <parse|e> a

pub type parse-error<a> 
  ParseOk( result: a, rest : sslice )
  ParseError( msg : string, rest : sslice )

pub effect parse
  fun satisfy( pred : sslice -> total maybe<(a,sslice)> ) : maybe<a>
  ctl fail(msg : string) : a
  ctl pick() : bool
  fun current-input() : sslice
  fun skip(n : int) : ()
  
pub fun parse( input0 : sslice, p : () -> <parse|e> a ) : e parse-error<a> 
  var input := input0
  handle p 
    return(x)
      ParseOk(x,input) 

    fun current-input()  
      input 

    final ctl fail(msg)     
      ParseError(msg,input) 

    fun satisfy(pred) 
      val inp = input
      val m   = fn() : total _ { pred(inp) }
      match m() 
        Just((x,cap)) -> { input := cap; Just(x) }
        Nothing       -> Nothing

    ctl pick() 
      val save = input;
      match resume(True)
        ParseOk(x1,rest1) -> ParseOk(x1,rest1)
        err1 -> 
          input := save
          match resume(False)   // todo: limit lookahead?
            ParseOk(x2,rest2) -> ParseOk(x2,rest2) 
            _err2 -> err1  // todo: merge or pick closest?

    fun skip(n)
      input := input.advance(n)


// pub fun safe-many(p : parser<e,a>) : <parse|e> list<a>
//   var acc := []
//   var input0 := current-input()
//   var input := input0
//   var done := False
  
//   while {!done}
//     match parse(input, p)
//       ParseOk(x, rest) ->
//         input := rest
//         acc := Cons(x, acc)
//       ParseError(_, _) ->
//         done := True
        
//   val consumed = input0.count() - input.count()
//   skip(consumed)
//   acc.reverse

  
// Apart from the fact that it doesn't compile, is it even a good idea?
// I wouldn't be surprised if adding handlers adds unnecessary overhead to each call.
// On the other hand, you really have to intercept fail calls, and there is no other way
// to do that than adding a handler

// pub fun safe-many(p : () -> <parse|e> a) : <parse|e> list<a>
//   val (consumed, acc) = safe-many-rec(p, current-input(), current-input(), [])
//   skip(consumed)
//   acc.reverse

// fun safe-many-rec(p : () -> <parse|e> a, input0 : sslice, input : sslice, acc : list<a>) : e (int, list<a>)
//   match parse(input, p)
//     ParseOk(x, rest) -> safe-many-rec(p, input0, rest, Cons(x, acc))
//     ParseError(_, _) -> (input0.count() - input.count(), acc)
  
    
// ---------------------------- COPIED FROM STD ------------------------------------


pub fun maybe( perr : parse-error<a> ) : maybe<a> 
  perr.either.maybe

pub fun either( perr : parse-error<a> ) : either<string,a> 
  match perr 
    ParseOk(x,_) -> Right(x)
    ParseError(msg,_) -> Left(msg)
  
pub fun parse-eof( input : sslice, p : () -> <parse|e> a) : e parse-error<a> 
  parse(input)
    val x = p()
    eof()
    x


pub fun (||)( p1 : parser<e,a>, p2 : parser<e,a> ) : <parse|e> a 
  if pick() then p1() else p2()

pub fun optional( default : a, p : parser<e,a> ) : <parse|e> a 
  p || (fn() default)

pub fun choose( ps : list<parser<e,a>> ) : <parse|e> a 
  match ps
    Nil         -> fail("no further alternatives")
    Cons(p,Nil) -> p()
    Cons(p,pp)  -> if pick() then p() else choose(pp)
  
pub fun satisfy-fail( msg : string, pred : sslice -> maybe<(a,sslice)> ) : parse a 
  match satisfy(pred)
    Nothing -> fail(msg)
    Just(x) -> x
  
pub fun eof() : parse () 
  match satisfy( fn(s) if s.is-empty then Just(((),s)) else Nothing ) 
    Nothing -> fail("expecting end-of-input")
    Just    -> ()
  
pub fun char-is( msg :string, pred : char -> bool ) : parse char 
  satisfy-fail(msg) fn(slice) 
    match slice.next
      Just((c,rest)) | pred(c) -> Just((c,rest))
      _ -> Nothing
    
  
fun next-while0( slice : sslice, pred : char -> bool, acc : list<char> ) : (list<char>,sslice) 
  match slice.next
    Just((c,rest)) | pred(c) -> next-while0(unsafe-decreasing(rest), pred, Cons(c,acc) )
    _ -> (acc.reverse,slice)
  

pub fun chars-are( msg :string, pred : char -> bool ) : parse list<char> 
  satisfy-fail(msg) fn(slice) 
    match slice.next-while0(pred,[]) 
      ([],_)    -> Nothing
      (xs,rest) -> Just((xs,rest))
    
fun next-match( slice : sslice, cs : list<char> ) : maybe<sslice> 
  match cs
    Nil -> Just(slice)
    Cons(c,cc) -> match slice.next 
      Just((d,rest)) | c==d -> rest.next-match( cc )
      _ -> Nothing
    
pub fun pstring( s : string ) : parse string 
  satisfy-fail(s) fn(slice) 
    match slice.next-match(s.list)
      Just(rest) -> Just((s,rest))
      Nothing    -> Nothing

pub fun char( c : char ) : parse char 
  char-is( show(c), fn(c0) c == c0  )

pub fun no-digit() : parse char 
  char-is("not a digit", fn(c) !c.is-digit )

pub fun digit() : parse int 
  val c = char-is("digit", is-digit)
  (c - '0').int

pub fun alpha() : parse char 
  char-is("alpha", is-alpha)

pub fun alpha-num() : parse char 
  char-is("alpha-num", is-alpha-num)

pub fun white() : parse char 
  char-is("", is-white)

pub fun whitespace() : parse string 
  chars-are("", is-white).string  

pub fun whitespace0() : parse string 
  optional("", whitespace)

pub fun digits() : parse string 
  chars-are("digit", is-digit ).string

pub fun digits0() : parse string 
  optional("0", digits)

pub fun sign() : parse bool 
  val c = one-of-or("+-", '+')
  (c=='-')

pub fun pnat() : parse int 
  digits().parse-int-default(0)

pub fun pint() : parse int 
  val neg = sign()
  val i = pnat()
  if neg then ~i else i

pub fun none-of( chars : string ) : parse char 
  char-is("", fn(c) !chars.contains(c.string) )

pub fun none-of-many1( chars : string ) : parse string 
  chars-are("", fn(c) !chars.contains(c.string)).string

pub fun one-of( chars : string ) : parse char 
  char-is(chars, fn(c) chars.contains(c.string))

pub fun one-of-or( chars : string, default : char ) : parse char 
  optional(default){ one-of(chars) }
  
pub fun hex-digits() : parse string 
  chars-are("digit", is-hex-digit ).string

