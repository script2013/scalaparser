package parsers

import scala.util
import java.util.Scanner
import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class ArrayCharSequence(arrStart: Int, arrEnd: Int, arr: Array[Char]) extends CharSequence{
  val len = arrEnd - arrStart
  
  override def length() = len
  
  override def charAt(index: Int): Char = {
    val pos = arrStart + index
    if (pos >= arrEnd)
      throw new IndexOutOfBoundsException()
    arr(pos) 
  }
  
  override def subSequence(start: Int, end: Int) = { 
    //to do:
    new ArrayCharSequence(start + arrStart, end + arrStart, arr)
  }
  
  override def toString: String = arr.slice(arrStart, arrEnd).mkString
}

object Parsers {

  case class ParserState(buffer: Array[Char], inputPos: Int) {
    val pos = getNonWhitespacePos(inputPos)
    
    def matchEOF: Option[ParserState] = {
        if (pos >= buffer.length)
          Some(this)
        else
          None
    }
    
    def matchString(expected: Array[Char]): Option[ParserState] = {
      if (pos + expected.length > buffer.length) {
        None
      }
      else{
        var i = pos
        var j = 0
        while (j < expected.length && expected(j) == buffer(i)) {
          i += 1; j += 1
        }
        if (j == expected.length) Some(newState(i)) else None
      }
    }
    
    def skipWhiteSpace(): ParserState = {
      var i = pos
      while(i < buffer.length && {val ch = buffer(i); ch == ' ' || ch == '\t'}){
        i += 1
      }
      newState(i)
    }
    
    def getNonWhitespacePos(pos: Int): Int = {
      var i = pos
      while(i < buffer.length && {val ch = buffer(i); ch == ' ' || ch == '\t'}){
        i += 1
      }
      i
    }
    
    def matchInt(): Option[(ParserState,Int)] = {
        var i = pos
        if (i < buffer.length && buffer(i) == '-'){
          i += 1
        }

        while (i < buffer.length && Character.isDigit(buffer(i))){
          i += 1
        }
        
        try{
          val subArray = buffer.slice(pos, i).mkString
          val value = Integer.parseInt(subArray)
          Some(newState(i), value)
        }
        catch{
          case e: NumberFormatException => None
          case o: Throwable => throw o
        }
    }
    
    def matchRegex(pattern: Regex): Option[(ParserState, String)] = {
        val subSeq = new ArrayCharSequence(pos, buffer.length, buffer)
        //val subSeq = buffer.slice(pos, buffer.length).mkString
        pattern.findPrefixOf(subSeq) match {
          case Some(value) => Some(newState(pos + value.length()), value)
          case None => None
        }
    }

    def newState(pos: Int) = ParserState(buffer, pos)
  }

  object ParserState {
    def init(input: String): ParserState = {
      ParserState(input.toCharArray, 0)
    }
  }

  trait Expr
  case class Number(val value: Int) extends Expr
  case class Ident(val name: String) extends Expr
  
  case class BinOp(val name: String, val operand1: Expr, val operand2: Expr) extends Expr
  case class UnaryOp(val name: String, val operand: Expr) extends Expr
  case class TupleN(val operands: List[Expr]) extends Expr
  case class FunApp(val expr: Expr, val operands: List[Expr]) extends Expr{
    //def this(name: Expr) = this(name, List())
  }
  case class AnonFun(val paramNames: List[String], val body: Expr) extends Expr
  
  def exprParser() = {
    val pIdent: Parser[Expr] = pRegexp("""[a-zA-Z]+""").bind(ident => pReturn(Ident(ident)))
    var pExpr: Parser[Expr] = null
    val pNumber: Parser[Expr] = pInt.bind(num => pReturn(Number(num)))
    val pUnaryOp: Parser[Expr] = 
        pString("-").bind(unaryOpName =>
        pExpr.bind(expr =>
        pReturn(UnaryOp(unaryOpName,expr))))
    
    val bracketedExpr = pString("(").bind(_ =>
                        pExpr.bind(expr =>
                        pString(")").bind(_ =>
                        pReturn(expr))))
                        
    val pFactor: Parser[Expr] = pChoice4(pIdent, pNumber, pUnaryOp, bracketedExpr)
    
    val pMultDiv = pChoice2(pString("*"), pString("/"))
    
    //pTerm = 
    /*
    def pTermParser(input: Option[(String, Expr)]): Parser[Expr] = {
            def mkBinOp(expr: Expr) = 
              input match {case Some((opName, expr0)) => BinOp(opName, expr0, expr)
                           case None => expr}  
            pFactor.bind(fac =>
            pChoice2(pMultDiv.bind(opName => 
                     pTermParser(Some(opName, mkBinOp(fac)))),
                     pReturn(mkBinOp(fac))))}*/
    
    /*
    def pLoopParser(pBaseParser: Parser[Expr], pOpParser: Parser[String]) = {
      def loop (input: Option[(String, Expr)]): Parser[Expr] = {
            def mkBinOp(expr: Expr) = 
              input match {case Some((opName, expr0)) => BinOp(opName, expr0, expr)
                           case None => expr}  
            pBaseParser.bind(fac =>
            pChoice2(pOpParser.bind(opName => 
                     loop(Some(opName, mkBinOp(fac)))),
                     pReturn(mkBinOp(fac))))}
      loop _}
    */
    
    def pRecSkel(pBaseParser: Parser[Expr], pOpParser: Parser[String]): Parser[Expr] =
        pBaseParser.bind(baseResult => {
        var expr: Expr = baseResult
        pWhile (pOpParser) (opName => 
                pBaseParser.bind(expr2 =>{
                expr = BinOp(opName, expr, expr2)
                pReturn(())})).bind(res =>
        pReturn(expr))})
    /*
        val!  baseResult = pBaseParser
        var expr: Expr = baseResult
        pWhile (val! opName = pOpParser){
          val! expr2 = pBaseParser
          expr = BinOp(opName, expr, expr2)
        }
        return(expr)
     */
    val pTermParser = pRecSkel(pFactor,pMultDiv)
    
    val pTerm: Parser[Expr] = pTermParser // (None)
    val pPlusMinus = pChoice2(pString("+"), pString("-"))
    /*
    pExpr = pTerm.bind(term =>
            pChoice(pPlusMinus.bind(opName =>
                    pExpr.bind(expr => pReturn(BinOp(opName, term, expr)))),
                    pReturn(term)))
    */                
    val pExprParser = pRecSkel(pTerm, pPlusMinus)
    pExpr = pExprParser //(None)
    
    val pFullExpr = pExpr.bind(expr => pEOF.bind(_ => pReturn(expr)))
    
    def parseExpr(input: String) = run(pFullExpr)(input)
    val r = parseExpr("sin()* ")
    println("Expr: " + r)
    ()
  }

  def exprParser1() = {
    val (pExprRef, pExpr) = pRef[Expr]()
    //var pExprRef: Parser[Expr] = null //forward ref
    //def pExpr = pExprRef
    
    
    val pIdentName = pRegexp("""[a-zA-Z]+""")
    val pIdent: Parser[Expr] = pIdentName.bind(ident => pReturn(Ident(ident)))

    val pComma: Parser[Unit] = pString(",").bind(_ => pReturn(()))
    val pOpenBracket = pString("(")
    val pCloseBracket = pString(")")

    /*
    val pFunApp: Parser[Expr] = 
      pIdentName.bind(name =>  //ident
      pOpenBracket.bind(_ =>   //(
      pChoice2(pCloseBracket.bind(_ => pReturn(new FunApp(name))), //if ) then done
               pSepBy1(pExpr, pComma).bind(exprs =>
               pCloseBracket.bind(_ => 
               pReturn(FunApp(name, exprs)))))))
    */
    
    def pCommaSep[A](expr: Parser[A]): Parser[List[A]] = {
               val manyArgs =  pSepBy1(expr, pComma).bind( exprs =>
                               pCloseBracket.bind(_ =>
                               pReturn(exprs) ) )
               pChoice2(pCloseBracket.bind(_ => pReturn(List())), //if ) then done
                        manyArgs) }
      
    def pFunAppArgs: Parser[List[Expr]] = pCommaSep(pExpr)
      
    def pKeyword(kw: String) = pIdentName.bind(name => if (name == kw) pReturn(Unit) else pFail(kw))
    
    def anonFunc = {
      pKeyword("fun").bind(_ =>
      pOpenBracket.bind(_ =>
      pCommaSep(pIdentName).bind(params =>
      pString("->").bind(_ =>
      pExpr.bind(expr =>
      pReturn(AnonFun(params, expr)))))))
    }
  
    /*
    val pFunAppOrIdent: Parser[Expr] = 
      pIdentName.bind(name =>  //ident
      pIf(pOpenBracket.bind(_=> pReturn(Unit)),
         pFunAppArgs.bind(args => pReturn(FunApp(Ident(name), args))),
         pEmpty.bind(_=> pReturn(Ident(name))) ) )
    */
    
    val pNumber: Parser[Expr] = pInt.bind(num => pReturn(Number(num)))

    val pUnaryOp: Parser[Expr] = 
        pString("-").bind(unaryOpName =>
        pExpr.bind(expr =>
        pReturn(UnaryOp(unaryOpName,expr))))

    def pExpon(pFactor: Parser[Expr]): Parser[Expr] = 
        pFactor.bind(expr =>
        pIf(pString("^"),
            pExpr.bind(power=> pReturn(BinOp("exp", expr, power))),
            pEmpty.bind(_ => pReturn(expr))))
        
    val bracketedExpr = pString("(").bind(_ =>
                        pExpr.bind(expr =>
                        pString(")").bind(_ =>
                        pReturn(expr))))
                        
    val bracketedExprOrTuple =
      pString("(").bind(_ =>
      pFunAppArgs.bind(exprs => {
      val expr = if (exprs.length == 1) exprs(0) else TupleN(exprs)
      pReturn(expr)}))
      
    val pFactor: Parser[Expr] = pChoice5(anonFunc, pIdent /* pFunAppOrIdent*/, pNumber, pUnaryOp, bracketedExprOrTuple)
    //def pFactor: Parser[Expr] = pExpon(pFactor0) // pChoice5(anonFunc, pFunAppOrIdent, pNumber, pUnaryOp, bracketedExprOrTuple)
    
    def pRecSkel(pBaseParser: Parser[Expr], pOpParser: Parser[String]): Parser[Expr] = {
    /*  val  baseResult = parse(pBaseParser)
        var expr: Expr = baseResult
        pWhile (val opName = parse(pOpParser)){
          val expr2 = parse(pBaseParser)
          expr = BinOp(opName, expr, expr2)
        }
        return(expr)
     */
      pBaseParser.bind(baseResult => {
        var expr: Expr = baseResult
        pWhile (pOpParser) (opName => 
                pBaseParser.bind(expr2 =>{
                expr = BinOp(opName, expr, expr2)
                pReturn(())})).bind(res =>
        pReturn(expr))})
    }

    def pRecFunApp(pBaseParser: Parser[Expr], pNextParser: Parser[List[Expr]], pOpParser: Parser[String]): Parser[Expr] = {
    /*  val  baseResult = parse(pBaseParser)
        var expr: Expr = baseResult
        pWhile (val opName = parse(pOpParser)){
          val expr2 = parse(pBaseParser)
          expr = BinOp(opName, expr, expr2)
        }
        return(expr)
     */
      pBaseParser.bind(baseResult => {
        var expr: Expr = baseResult
        pWhile (pOpParser) (opName => 
                pNextParser.bind(expr2 =>{
                expr = FunApp(expr, expr2) //BinOp(opName, expr, expr2)
                pReturn(())})).bind(res =>
        pReturn(expr))})
    }
    
    //val pExp =pString("^")
    val pExpTerm = pRecFunApp(pFactor, pFunAppArgs, pOpenBracket) //pRecSkel(pFactor,pExp)
    
    val pMultDiv = pChoice2(pString("*"), pString("/"))
    val pPlusMinus = pChoice2(pString("+"), pString("-"))
    
    val pTerm = pRecSkel(pExpTerm /*pFactor*/,pMultDiv)
    val pResolvedExpr = pRecSkel(pTerm, pPlusMinus)
    //leftAssoc(leftAssoc(leftAssoc(..., funApp), 
    pExprRef.set(pResolvedExpr)
    
    val pFullExpr = pExpr.bind(expr => pEOF.bind(_ => pReturn(expr)))


    /*
    val res = pChoice2({for (_ <- pCloseBracket) yield Ident("a")},
                      {for (_ <- pCloseBracket) yield Ident("")});
    
    def pFunApp = for(
      name <- pIdentName;
      _ <- pOpenBracket;
    ) yield res; // {notImpl()}
    */
    /*
    val pFunApp = 
      pIdentName.bind(name =>  //ident
      pOpenBracket.bind(_ =>   //(
      pChoice2(pCloseBracket.bind(_ => pReturn(new FunApp(name))), //if ) then done
              { val args = ArrayBuffer[Expr]()
                pExpr.bind(expr => { //parse expr
                args += expr
                pWhile(pString(","))(_ => //
                pExpr.bind(expr => {
                args += expr
                pReturn(Unit) } ) ) } ).bind(_ =>
                pCloseBracket.bind(_ =>
                pReturn(FunApp(name, args.toList) )  
                ) ) } ) ) )

    */
    //(...)(...)(...)
    //[], {a -> b, c -> d}
    def parseExpr(input: String) = run(pFullExpr)(input)
    val r = parseExpr("(fun (a, c) -> fun (b) -> (a+5,b,c))(1,3)(2)") //"(fun (a) -> a + b)(1,2)") //("f(1,(2 + 1, 3))") //("b + f(a, g(c)) ")

    println("Expr: " + r)
  }
 
  
  def test() {
    val s = ParserState.init("input")
    println("regex" + s.matchRegex("input".r))
    
    val p1 = pString("inp")
    val p2 = pString("ut")
    val p3 = p1 ==> p2 ==> p1 //chain(p1,p2)
    def f(xy: (Unit, Unit)): Unit = ()
    //val p4 = map(p3, f)
    //java.lang.Integer.parseInt(arg0)
    val p5 = pString("inp").bind(result0 =>
             pString("up").bind(result1 =>
             pReturn((result0, result1))))
               
    val p4 = p3 map (_ => Unit) // ((xy: (Unit,Unit)) => Unit)
    val r = run(p5)("input")
    val l = List(1)
   
    val x: Parser[Unit] = 
      for(
          r0 <- pString("inp");
          r1 <- pString("up")) 
      yield 
        {(r0, r)}
    
    //val s0 = ParserState.init("-2-101s")
    //println(s0.matchInt())
    val pPlus = pString("+")
    val pMinus = pString("-")
    val pOp = pChoice2(pPlus, pMinus)
    val ident = pRegexp("""[a-zA-Z]+""")
    
    val p6 = for(
      a <- ident;
      op <- pOp;
      b <- ident
    )
    yield {(op, a, b)}
    
    val r6 = run(p6) ("a + b") //"("a - -6")
    println(r6)  
    //println(run(x)("input"))

    /*
    val q = "xinpu".toCharArray
    val res = s.matchString(q)
    println(res)

    val x: Option[ParserState] = s.matchString("".toCharArray())
    x match  {
      case Some(newState) => ParserSuccess((), newState)
      case None => failWith(s.pos, "pString")
    } 
    */
  }

  case class ParserError(pos: Int, msg: String)

  sealed abstract class ParserResult[+A]

  final case class ParserSuccess[+A](result: A, next: ParserState) extends ParserResult[A]

  final case class ParserFailure(error: ParserError) extends ParserResult[Nothing]

  def failWith(pos: Int, message: String): ParserResult[Nothing] = ParserFailure(ParserError(pos, message))
  def notImpl() = failWith(0, "na")

  case class Parser[+A](code: ParserState => ParserResult[A]) {
    def run(state: ParserState): ParserResult[A] = code(state)
    
    def map[B](f: A => B): Parser[B] = Parsers.map(this, f)
    
    def ==>[B](p2: Parser[B]): Parser[(A,B)] = Parsers.chain(this, p2)
   
    def bind[B](f: A => Parser[B]): Parser[B] = Parser(state =>
       run(state) match {
        case ParserSuccess(result, nextState) => f(result).run(nextState)
        case failure@ParserFailure(_) => failure
       })
       
    def flatMap[B](f: A => Parser[B]):  Parser[B] = bind(f)
   

       
  }
  
  class ParserRef[A](){
    var p: Parser[A] = null
    def get = p
    def set(p: Parser[A]){this.p = p}
  }  

  def pReturn[A](value: A): Parser[A] = Parser(state => ParserSuccess(value, state))
  def pFail(msg: String): Parser[Nothing] = Parser(state => ParserFailure(ParserError(state.pos, msg)))
  /*
  def pWhile1[A] (cond: Parser[A]) (body: A => Parser[Unit]): Parser[Unit] =  Parser(state =>
     cond.run(state) match {
      case ParserSuccess(condResult, nextState) => 
        body(condResult).run(nextState) match {
          case ParserSuccess(_, nextStateBody) => pWhile1(cond)(body).run(nextStateBody)
          case failure@ParserFailure(_) => failure
        }
      case failure@ParserFailure(_) => ParserSuccess((), state)
     })
   */  


  
  def pRef[A](): (ParserRef[A], Parser[A]) = {
    val pRef = new ParserRef[A]
    (pRef, Parser(state => pRef.get.run(state)))
  }
  
  
  def pSepBy1[A] (pItem: Parser[A], pSep: Parser[Unit]): Parser[List[A]] =
                pItem.bind(expr => { //parse expr
                val args = ArrayBuffer[A]()
                args += expr
                (pWhile(pSep)(_ => //
                pItem.bind(expr => {
                args += expr
                pReturn(Unit) } ) )).bind(_ => pReturn(args.toList) ) } )
     
  def pWhile[A] (cond: Parser[A]) (body: A => Parser[Unit]): Parser[Unit] = { 
      @tailrec
      def loop(state: ParserState): ParserResult[Unit] = 
        cond.run(state) match {
          case ParserSuccess(condResult, nextState) => 
                  body(condResult).run(nextState) match {
                      case ParserSuccess(_, nextStateBody) => loop(nextStateBody)
                      case failure@ParserFailure(_) => failure
                  }
          case failure@ParserFailure(_) => ParserSuccess((), state) 
        }
      Parser(loop)}

  /*
  def pDoWhile[A] (body: Parser[A]) (cond: Parser[Unit]) = {
    
  }*/
  
  def pChoice2[A <: C, B <: C, C](p0: Parser[A], p1: Parser[B]): Parser[C] = Parser(state =>
        p0.run(state) match {
          case success@ParserSuccess(_, _) => success
          case ParserFailure(error0) =>
            p1.run(state) match {
              case success@ParserSuccess(_,_) => success
              case ParserFailure(error1) => ParserFailure(error1)
            }
        })

        
  //def pIf[C, A <: R, B <: R, R] (pCond: Parser[C], pTrue: C -> Parser[A], pFalse: Parser[B]): Parser[R] = 
        
  def pIf[C, A <: R, B <: R, R] (pCond: Parser[C], pTrue: Parser[A], pFalse: Parser[B]): Parser[R] = 
    Parser(state =>
        pCond.run(state) match {
          case ParserSuccess(_, state1) => pTrue.run(state1)
          case ParserFailure(_) => pFalse.run(state) 
        }      
    )
    
  
  def pChoice3[A0 <: R, A1 <: R, A2 <: R, R](p0: Parser[A0], p1: Parser[A1], p2: Parser[A2]): Parser[R] =
    pChoice2(p0, pChoice2(p1, p2))
 
  def pChoice4[A0 <: R, A1 <: R, A2 <: R, A3 <: R, R](p0: Parser[A0], p1: Parser[A1], p2: Parser[A2],  p3: Parser[A3]): Parser[R] =
    pChoice2(p0, pChoice3(p1, p2, p3))    

  def pChoice5[A0 <: R, A1 <: R, A2 <: R, A3 <: R, A4 <: R, R](p0: Parser[A0], p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], p4: Parser[A4]): Parser[R] =
    pChoice2(p0, pChoice4(p1, p2, p3, p4))   
    
  def pChoice[A](ps: Parser[A]*): Parser[A] = Parser(state => {
    val iter = ps.iterator
    var result: ParserResult[A] = failWith(state.pos, "pChoice")
    var done = false
    while (!done && iter.hasNext){
      val p = iter.next()
      p.run(state) match {
        case success@ParserSuccess(_,_) => {done = true; result = success}
        case ParserFailure(error) => {/*merge erros*/}
      } 
    }
    result})
  
  def pString(expected: String): Parser[String] = {
    val expectedArr = expected.toCharArray()
    Parser(state => 
      state.matchString(expectedArr) match {
          case Some(newState) => ParserSuccess(expected, newState)
          case None => failWith(state.pos, "pString")
      }
    )
  }

  val pEmpty: Parser[Unit] = Parser(state => ParserSuccess((), state))
  
  val pEOF: Parser[Unit] = Parser(state => 
      state.matchEOF match {
          case Some(newState) => ParserSuccess((), newState)
          case None => failWith(state.pos, "pEOF")
      }
    )
    
  def pRegexp(regexp: String): Parser[String] = {
    val pattern = regexp.r
    Parser(state => 
      state.matchRegex(pattern) match {
          case Some((newState, found)) => ParserSuccess(found, newState)
          case None => failWith(state.pos, "pString")
      }
    )
  }
  
  val pInt: Parser[Int] =
    Parser(state => 
      state.matchInt match {
          case Some((newState,value)) => ParserSuccess(value, newState)
          case None => failWith(state.pos, "pInt")
      })
  
  val pWhitespaceAny = Parser(state =>
    ParserSuccess((), state.skipWhiteSpace)
  )
  
  def map[A, B](p: Parser[A], f: A => B): Parser[B] = Parser(state =>
    p.run(state) match{
      case ParserSuccess(result, state) => ParserSuccess(f(result), state)
      case failure@ParserFailure(_) => failure
    })
  
  def chain[A, B] (p1: Parser[A], p2: Parser[B]): Parser[(A,B)] = Parser(state0 =>
    p1.run(state0) match {
      case ParserSuccess(result1, state1) => 
        p2.run(state1) match {
          case ParserSuccess(result2, state2) => ParserSuccess((result1, result2), state2)
          case failure@ParserFailure(_) => failure
        }
      case failure@ParserFailure(_) => failure
    })

    
  def run[A](parser: Parser[A])(input: String): ParserResult[A] = {
    val state = ParserState(input.toCharArray(), 0)
    parser.run(state)
    //failWith(0, "")
  }
}
