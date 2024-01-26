import iter
import util


// Stream of tokens to be parsed.
// The stream has to be deterministic, in the sense that a stream generating function
// can be stored and will always yield the same tokens when called.
pub alias input<t> = iter<t,<>>

pub alias parser<t,a,e> = () -> <parse<t>|e> a
pub alias parse<t> = <div,ndparse<t>>

pub type parse-result<t,a> 
  ParseOk(result: a, rest : input<t>)
  ParseError(msg : string, rest : input<t>)

pub effect ndparse<t>
  fun satisfy(pred : input<t> -> total maybe<(a,input<t>)>) : maybe<a>
  ctl fail(msg : string) : a
  ctl pick() : bool
  fun get-input() : input<t>
  fun set-input(input : input<t>) : ()
  
pub fun parse(input0 : input<t>, p : parser<t,a,e>) : <div|e> parse-result<t,a> 
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

pub fun many(p : parser<t,a,e>) : <parse<t>|e> list<a>
  val input = get-input()
  val (acc, rest) = mask<ndparse>
    many-rec(p, input, [])
  set-input(rest)
  acc.reverse

fun many-rec(p : parser<t,a,e>, input : input<t>, acc : list<a>) : <div|e> (list<a>, input<t>)
  match parse(input, p)
    ParseOk(x, rest) -> many-rec(p, rest, Cons(x, acc))
    ParseError(_, _) -> (acc, input)
    
pub fun many1(p : parser<t,a,e>) : <parse<t>|e> list<a>
  Cons(p(), many(p))

// pub fun many(p : parser<t,a,e>) : <parse<t>|e> list<a>
//   val (acc, rest) = many-rec(p, get-input(), [])
//   set-input(rest)
//   acc.reverse

// fun many-rec(p : parser<t,a,e>, input : input<t>, acc : list<a>) : <parse<t>|e> (list<a>, input<t>)
//   match parse(input, p)
//     ParseOk(x, rest) -> many-rec(p, rest, Cons(x, acc))
//     ParseError(_, _) -> (acc, input)


// ---------------------------- FROM STD ------------------------------------


pub fun maybe(perr : parse-result<t,a>) : maybe<a> 
  perr.either.maybe

pub fun either(perr : parse-result<t,a>) : either<string,a> 
  match perr 
    ParseOk(x,_) -> Right(x)
    ParseError(msg,_) -> Left(msg)
  
pub fun eof() : <parse<t>|e> () 
  match satisfy(fn(s) if s.is-empty then Just(((),s)) else Nothing) 
    Nothing -> fail("expecting end-of-input")
    Just    -> ()
  
pub fun parse-to-eof(input : input<t>, p : parser<t,a,e>) : <div|e> parse-result<t,a> 
  parse(input)
    val result = p()
    eof()
    result

pub fun (||)(p1 : parser<t,a,e>, p2 : parser<t,a,e>) : <parse<t>|e> a 
  if pick() then p1() else p2()

pub fun optional(default : a, p : parser<t,a,e>) : <parse<t>|e> a 
  p || (fn() default)

pub fun choose(ps : list<parser<t,a,e>>) : <parse<t>|e> a 
  match ps
    Nil         -> fail("no further alternatives")
    Cons(p,Nil) -> p()
    Cons(p,pp)  -> if pick() then p() else choose(pp)
  
pub fun satisfy-fail(msg : string, pred : input<t> -> maybe<(a,input<t>)>) : <parse<t>|e> a 
  match satisfy(pred)
    Nothing -> fail(msg)
    Just(x) -> x
  
pub fun token-is(msg : string, pred : t -> bool) : <parse<t>|e> t
  satisfy-fail(msg) fn(input)
    match input.next
      Nothing -> Nothing
      Just((t, rest)) -> if pred(t) then Just((t,rest)) else Nothing
  
fun next-while0(input : input<t>, pred : t -> bool, acc : list<t>) : (list<t>,input<t>) 
  match input.next
    Just((c,rest)) | pred(c) -> next-while0(unsafe-decreasing(rest), pred, Cons(c,acc))
    _ -> (acc.reverse, input)
  

pub fun tokens-are(msg : string, pred : t -> bool) : <parse<t>|e> list<t> 
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

  
// ---------------------- TOKENIZATION ----------------------------

fun tokenize(s : sslice) : yield<char> ()
  s.foreach fn(c)
    yield(c)
    
fun tokenize(s : string) : yield<char> ()
  s.slice.tokenize

fun tokenize(l : list<a>) : yield<a> ()
  l.foreach fn(t)
    yield(t)
    
// Necessary because otherwise the effect variable is interpreted differently or something
pub fun is-empty'(it : iter<a,<>>) : bool
  it.next.is-nothing

fun tokenize(p : parser<t,a,<>>, input : input<t>, show : a -> string) : <div,exn,yield<a>> ()
  match parse(input, p)
    ParseOk(a, rest) ->
      // TODO: assert advance
      debug(show(a))
      yield(a)
      tokenize(p, rest, show)
    ParseError(msg, rest) -> 
      if !rest.is-empty' then
        throw("Error during tokenization: " ++ msg)


// ---------------------- EXAMPLES ----------------------------

pub value type dsv-token
  Value(s : string)
  Sep
  Newline

pub fun show(t : dsv-token) : string
  match t
    Value(s) -> "val: " ++ s
    Sep -> " | "
    Newline -> " lf\n"

pub fun show(dsv : list<dsv-token>) : string
  dsv.map(show).join()

fun parse-tsv-value() : parse<char> dsv-token
  val chars = many1({ none-of("\t\r\n") })
  Value(chars.string)

fun parse-tsv-sep() : parse<char> dsv-token
  val _ = char('\t')
  Sep

fun parse-newline() : parse<char> dsv-token
  val _ = char('\n')
  Newline

fun parse-tsv-token() : parse<char> dsv-token
  parse-tsv-value || ({parse-tsv-sep || parse-newline})

pub fun tokenize-tsv(input : input<char>) : <div,exn,yield<dsv-token>> ()
  tokenize(parse-tsv-token, input, show)
  
pub fun main()
  val input = iter({"1".tokenize})
  
  // println("Input:")
  // val s = input.list.show
  // val l = input.list
  // println(l.show)
  
  val result = iter(fn() tokenize-tsv(input))
  println("Output:")
  println(result.list.show)
