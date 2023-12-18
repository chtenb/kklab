import yield


// Stream of tokens to be parsed.
// The stream has to be deterministic, in the sense that a stream generating function
// can be stored and will always yield the same tokens when called.
pub alias input<t> = iter<t,<>>

pub alias parser<t,a> = () -> <parse<t>> a

pub type parse-result<t,a> 
  ParseOk(result: a, rest : input<t>)
  ParseError(msg : string, rest : input<t>)

pub effect parse<t>
  fun satisfy(pred : input<t> -> total maybe<(a,input<t>)>) : maybe<a>
  ctl fail(msg : string) : a
  ctl pick() : bool
  fun get-input() : input<t>
  fun set-input(input : input<t>) : ()
  
pub fun parse(input0 : input<t>, p : parser<t,a>) : div parse-result<t,a> 
  var input := input0
  handle p 
    return(x)
      ParseOk(x,input) 

    fun get-input()  
      input 
    
    fun set-input(new)  
      input := new

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

pub fun many(p : parser<t,a>) : <div,parse<t>> list<a>
  val (acc, rest) = many-rec(p, get-input(), [])
  set-input(rest)
  acc.reverse

fun many-rec(p : parser<t,a>, input : input<t>, acc : list<a>) : div (list<a>, input<t>)
  match parse(input, p)
    ParseOk(x, rest) -> many-rec(p, rest, Cons(x, acc))
    ParseError(_, _) -> (acc, input)

fun tokenize(s : sslice) : yield<char> ()
  s.foreach fn(c)
    yield(c)
    

// ---------------------------- COPIED FROM STD ------------------------------------


pub fun maybe(perr : parse-result<t,a>) : maybe<a> 
  perr.either.maybe

pub fun either(perr : parse-result<t,a>) : either<string,a> 
  match perr 
    ParseOk(x,_) -> Right(x)
    ParseError(msg,_) -> Left(msg)
  
pub fun eof() : parse<t> () 
  match satisfy(fn(s) if s.is-empty then Just(((),s)) else Nothing) 
    Nothing -> fail("expecting end-of-input")
    Just    -> ()
  
pub fun parse-eof(input : input<t>, p : parser<t,a>) : div parse-result<t,a> 
  parse(input)
    val result = p()
    eof()
    result

pub fun (||)(p1 : parser<t,a>, p2 : parser<t,a>) : parse<t> a 
  if pick() then p1() else p2()

pub fun optional(default : a, p : parser<t,a>) : parse<t> a 
  p || (fn() default)

pub fun choose(ps : list<parser<t,a>>) : parse<t> a 
  match ps
    Nil         -> fail("no further alternatives")
    Cons(p,Nil) -> p()
    Cons(p,pp)  -> if pick() then p() else choose(pp)
  
pub fun satisfy-fail(msg : string, pred : input<t> -> maybe<(a,input<t>)>) : parse<t> a 
  match satisfy(pred)
    Nothing -> fail(msg)
    Just(x) -> x
  
pub fun token-is(msg : string, pred : t -> bool) : parse<t> t
  satisfy-fail(msg) fn(input)
    match input.next
      Nothing -> Nothing
      result as Just((t, _)) -> if pred(t) then result else Nothing
  
fun next-while0(input : input<t>, pred : t -> bool, acc : list<t>) : (list<t>,input<t>) 
  match input.next
    Just((c,rest)) | pred(c) -> next-while0(unsafe-decreasing(rest), pred, Cons(c,acc))
    _ -> (acc.reverse, input)
  

pub fun tokens-are(msg : string, pred : t -> bool) : parse<t> list<t> 
  satisfy-fail(msg) fn(input) 
    match input.next-while0(pred,[]) 
      ([],_)    -> Nothing
      (xs,rest) -> Just((xs,rest))
    
fun next-match(input : input<char>, cs : list<char>) : maybe<input<char>> 
  match cs
    Nil -> Just(input)
    Cons(c,cc) -> match input.next
      Just((d,rest)) | c==d -> rest.next-match(cc)
      _ -> Nothing
    
pub fun pstring(s : string) : parse<char> string 
  satisfy-fail(s) fn(slice) 
    match slice.next-match(s.list)
      Just(rest) -> Just((s,rest))
      Nothing    -> Nothing

pub fun char(c : char) : parse<char> char 
  token-is(show(c), fn(c0) c == c0 )

pub fun no-digit() : parse<char> char 
  token-is("not a digit", fn(c) !c.is-digit)

pub fun digit() : parse<char> int 
  val c = token-is("digit", is-digit)
  (c - '0').int

pub fun alpha() : parse<char> char 
  token-is("alpha", is-alpha)

pub fun alpha-num() : parse<char> char 
  token-is("alpha-num", is-alpha-num)

pub fun white() : parse<char> char 
  token-is("", is-white)

pub fun whitespace() : parse<char> string 
  tokens-are("", is-white).string  

pub fun whitespace0() : parse<char> string 
  optional("", whitespace)

pub fun digits() : parse<char> string 
  tokens-are("digit", is-digit).string

pub fun digits0() : parse<char> string 
  optional("0", digits)

pub fun sign() : parse<char> bool 
  val c = one-of-or("+-", '+')
  (c=='-')

pub fun pnat() : parse<char> int 
  digits().parse-int-default(0)

pub fun pint() : parse<char> int 
  val neg = sign()
  val i = pnat()
  if neg then ~i else i

pub fun none-of(chars : string) : parse<char> char 
  token-is("", fn(c : char) !chars.contains(c.string))

pub fun none-of-many1(chars : string) : parse<char> string 
  tokens-are("", fn(c : char) !chars.contains(c.string)).string

pub fun one-of(chars : string) : parse<char> char 
  token-is(chars, fn(c : char) chars.contains(c.string))

pub fun one-of-or(chars : string, default : char) : parse<char> char 
  optional(default){ one-of(chars) }
  
pub fun hex-digits() : parse<char> string 
  tokens-are("digit", is-hex-digit).string

