package parsers

object Parsers {
import scala.util
import java.util.Scanner
import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

  class Loc(val fromPos: Int, val toPos: Int){
	override def toString() = s"Loc($fromPos, $toPos)"
  }
  
  object Loc{
	def apply(fromPos: Int, toPos: Int): Loc = new Loc(fromPos, toPos)
	def combine(fromLoc: Loc, toLoc: Loc): Loc = new Loc(fromLoc.fromPos, toLoc.toPos)
  }

/*
  class ParseInfo[+A](val value: A){
	def loc: Loc = Loc(0, 0)
  }

  object ParseInfo{
    def apply[A](value: A): ParseInfo[A] = new ParseInfo(value)
    def apply[A](value: A, loc: Loc): ParseInfo[A] = new ParseInfo(value)    
  }
  */
  case class ParserState(buffer: ArrayCharSequence, inputPos: Int) {
    val pos = getNonWhitespacePos(inputPos)
    def getPos = pos
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
      //to do: move to separate method
      while(i < buffer.length && {val ch = buffer(i); ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'}){
        i += 1
      }
      if (i + 1 < buffer.length && buffer(i) == '/' && buffer(i + 1) == '*'){
    	  i += 2
      
    	  var done = false
	      while (i + 1 < buffer.length && !done){
	        if (buffer(i) == '*' && buffer(i + 1) == '/'){
	          i += 2;
	          done = true;
	        }
	        else{
	          i += 1
	        }
	      }
	  
	      //to do: move to separate method
	      while(i < buffer.length && {val ch = buffer(i); ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'}){
	        i += 1
	      }        
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
          val intSubstr = buffer.subSequence(pos, i).toString
          val value = Integer.parseInt(intSubstr)
          Some(newState(i), value)
        }
        catch{
          case e: NumberFormatException => None
          case o: Throwable => throw o
        }
    }
    
    def matchRegex(pattern: Regex): Option[(ParserState, String)] = {
        val subSeq = buffer.subSequence(pos, buffer.length)
        //val subSeq = buffer.slice(pos, buffer.length).mkString
        pattern.findPrefixOf(subSeq) match {
          case Some(value) => Some(newState(pos + value.length()), value)
          case None => None
        }
    }
    
    def matchQuotedString(): Option[(ParserState,String)] = {
    	println("matchQuotedString")
        var i = pos
        if (i < buffer.length && buffer(i) == '"'){
        	i += 1;
        	val (notDone, success, error) = (0,1,2)
        	var flag = notDone
        	val sb = new StringBuilder()
	        while (i < buffer.length && flag == notDone){
	        	if (buffer(i) == '\\'){
	        		i += 1;
	        		flag = error
	        		if (i < buffer.length){
	        			val ch = buffer(i)
	        			if (ch == 'n' || ch == '"'){
	        			  sb.append(if (ch == 'n') '\n' else ch)
	        			  i += 1;
	        			  flag = notDone
	        			}
	        		} 
	        		
	        	}
	        	else if (buffer(i) == '"'){
	        	  flag = success
	        	   i += 1
	        	}
	        	else{
	        	  sb.append(buffer(i))
	        	  i += 1
	        	}
	        }
        	if (flag == success){
        		Some(newState(i), sb.toString())
        	}
        	else
        	  None
        }
        else 
          None
    }

    def newState(pos: Int) = ParserState(buffer, pos)
  }

  object ParserState {
    def init(input: String): ParserState = {
      ParserState(ArrayCharSequence.fromString(input), 0)
    }
  }


  case class ParserError(pos: Int, stack: List[String]){
    def printMessage(input: String){
    	def defaultIfNotFound(defaultValue: Int, value: Int) = if (value == -1) defaultValue else value
    	val startPos = 1 + input.lastIndexOf("\n", pos)
    	val untilPos = defaultIfNotFound(input.length, input.indexOf("\n", pos))
    	println("Error at pos: " + pos)
    	println(input.substring(startPos, untilPos))
    
    	val fixedPos = pos //if (pos < input.length && input(pos) == ' ') input.lastIndexWhere(ch => ch != ' ', pos) else pos
    	  //if (pos > 0) (1 + input.lastIndexWhere(ch => ch != ' ', pos - 1)) else pos
    	for(i <- startPos until fixedPos){
    	  print(".")
    	}
    	println("^")
    	println("Expected: " + stack.reverse.mkString(", "))
    }
  }

  sealed abstract class ParserResult[+A]{
    def success: A
    def failure: ParserError
    def isSuccess: Boolean
  }

  case class ParserSuccess[+A](result: A, next: ParserState) extends ParserResult[A]{
        def success: A = result
        def failure: ParserError = throw new Exception("parser succeeded")
        def isSuccess = true
  }
  
  class ParserSuccessWithLoc[+A](result: A, next: ParserState, loc: Loc) extends ParserSuccess[A](result, next)

  final case class ParserFailure(error: ParserError) extends ParserResult[Nothing]{
        def success: Nothing = throw new Exception("Parser failed at pos " + error.pos + " with errors: " + error.stack.reverse.mkString(", "))
        def failure: ParserError = error
        def isSuccess = false
  }
  
  def mergeFailures[A <: C, B <: C, C](prevError: ParserResult[A], lastError: ParserResult[B]): ParserResult[C] = {
	if (prevError.failure.pos < lastError.failure.pos){
		 lastError
	}
	else if (prevError.failure.pos > lastError.failure.pos){
		prevError
	}
	else{
		val stack = prevError.failure.stack ++ lastError.failure.stack
		ParserFailure(ParserError(Math.max(prevError.failure.pos, lastError.failure.pos), stack))
	}
  }
  
  def failWith(pos: Int, trace: String): ParserResult[Nothing] = ParserFailure(ParserError(pos, List(trace)))
  def failWith0(pos: Int): ParserResult[Nothing] = ParserFailure(ParserError(pos, List()))
  
  def notImpl() = failWith(0, "notImpl")

  case class Parser[+A](code: ParserState => ParserResult[A]) {
    def run(state: ParserState): ParserResult[A] = code(state)
    
    def map[B](f: A => B): Parser[B] = Parsers.map(this, f)
    
    def bind[B](f: A => Parser[B]): Parser[B] = Parser(state =>
       run(state) match {
        case ParserSuccess(result, nextState) => {
          //println("->" + result + " state:" + nextState.inputPos)
          f(result).run(nextState)
        }
        case failure@ParserFailure(ParserError(pos,msg)) => {
          //println("Failure: " + msg.mkString(", ") + " state:" + pos)
          failure
        }
       })
    
    def ignore(): Parser[Unit] = pEmpty.bind(res => pReturn(Unit))
    
    def flatMap[B](f: A => Parser[B]):  Parser[B] = bind(f)
    //def ==>[B](p2: Parser[B]): Parser[(A,B)] = Parsers.chain(this, p2)
  }
  
  class ParserRef[A](){
    var p: Parser[A] = Parser(state => this.get.run(state))
    def get = p
    def set(p: Parser[A]){this.p = p}
  }  

  def pReturn[A](value: A): Parser[A] = Parser(state => ParserSuccess(value, state))
  
  def pWithLocation[A, B](p: Parser[A], f:(A, Loc) => B): Parser[B] = Parser(state => {
    val startPos = state.getPos
   	p.run(state) match{
      case ParserSuccess(result, state) => {
    	val endPos = state.getPos
    	val loc = Loc(startPos, endPos)
        ParserSuccess(f(result, loc), state)
      }
      case failure@ParserFailure(_) => failure
    }})
  
  //def pReturn[A](value: A, loc: Loc): Parser[A] = Parser(state => ParserSuccess(ParseInfo(value, loc), state))
  def pReturnEmpty(): Parser[Unit] = pReturn(Unit)
  //def pReturnEmpty(loc: Loc): Parser[Unit] = pReturn(ParseInfo(Unit, loc))
  
  def pFail(msg: String): Parser[Nothing] = Parser(state => ParserFailure(ParserError(state.pos, List(msg))))
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
    (pRef, pRef.get)}
  
  /*
  def pSepBy1[A] (pItem: Parser[A], pSep: Parser[Unit]): Parser[List[A]] =
                pItem.bind(expr => { //parse expr
                val args = ArrayBuffer[A]()
                args += expr
                (pWhile0 ("pSepBy1") (pSep)(_ => //
                pItem.bind(expr => {
                args += expr
                pReturn(Unit) } ) )).bind(_ => pReturn(args.toList) ) } )*/
   
  def not[A](p: Parser[A]): Parser[Unit] = Parser(state =>
  	p.run(state) match{
  	case ParserSuccess(_,_) => ParserFailure(ParserError(state.pos, List()))
  	case ParserFailure(_) => ParserSuccess(Unit, state)
  	}
  )
  
  def lookAhead[A](p: Parser[A]): Parser[Unit] = Parser(state =>
  	p.run(state) match{
  	case ParserSuccess(_,_) => ParserSuccess(Unit, state) // ParserFailure(ParserError(state.pos, List()))
  	case failure@ParserFailure(_) => failure //ParserSuccess(Unit, state)
  	}
  )
  
  def pWhileInternal[A] (name: String) (cond: Parser[A]) (body: A => Parser[Unit]): Parser[Unit] = { 
      @tailrec
      def loop(state: ParserState): ParserResult[Unit] = 
        cond.run(state) match {
          case ParserSuccess(condResult, nextState) => 
                  body(condResult).run(nextState) match {
                      case ParserSuccess(_, nextStateBody) => loop(nextStateBody)
                      case failure@ParserFailure(_) => failure
                  }
          case failure@ParserFailure(_) => {
        	  //println("while: " + name + " pos: " + state.pos)
        	  ParserSuccess(Unit, state) 
          }
        }
      Parser(loop)}

  def pWhileList[A,B] (name: String) (cond: Parser[A]) (body: A => Parser[B]): Parser[List[(A,B)]] = { 
	    pEmpty.bind(_ =>
	    {val res = ArrayBuffer[(A,B)]()
		 (pWhileInternal (name) (cond) (it0 =>
		   				       body(it0).
		   				       bind(item => 
			  		           {res += ((it0,item)); 
			  		           pReturnEmpty}))).
	     bind(_=> pReturn(res.toList))}) 
	  }
     
  def pWhile[B] (cond: Parser[Unit]) (body: Parser[B]): Parser[List[B]] = { 
	    pEmpty.bind(_ =>
	    {val res = ArrayBuffer[B]()
		 (pWhileInternal ("") (cond) (it0 =>
		   				       body.bind(item => 
			  		           {res += item; 
			  		           pReturnEmpty}))).
	     bind(_=> pReturn(res.toList))}) 
	  }
  
  def pMany0[A] (p: Parser[A]): Parser[List[A]] =
  /*{
    pEmpty.bind(_ =>
    {val res = ArrayBuffer[A]()
	(pWhile ("many0") (p) (item => {res += item; pReturn(())})).bind(_=> pReturn(res.toList))}) 
  }*/
  
  //def pWhile[A] (name: String) (cond: Parser[A]) (body: A => Parser[Unit]): Parser[Unit] = 
	  { 
	      @tailrec
	      def loop(state: ParserState,lst: List[A]): ParserResult[List[A]] = 
	          p.run(state) match {
	              case ParserSuccess(res, nextStateBody) => loop(nextStateBody, lst ++ List(res))
	              case failure@ParserFailure(_) => failure
	          }
          Parser(s => loop(s, List()))
     }  
/*
  def pDoWhile[A] (body: Parser[A]) (cond: Parser[Unit]) = {
    
  }*/
  
  //to do: leave after pChoice(?*)
  /*
  def pChoice2[A <: C, B <: C, C](p0: Parser[A], p1: Parser[B]): Parser[C] = Parser(state =>
        p0.run(state) match {
          case success@ParserSuccess(_, _) => success
          case ParserFailure(error0) =>
            p1.run(state) match {
              case success@ParserSuccess(_,_) => success
              case ParserFailure(error1) => ParserFailure(error1)
            }
        })
   */
        
  //def pIf[C, A <: R, B <: R, R] (pCond: Parser[C], pTrue: C -> Parser[A], pFalse: Parser[B]): Parser[R] = 
        
  def pIf[C, A <: R, B <: R, R] (pCond: Parser[C], pTrue: Parser[A], pFalse: Parser[B]): Parser[R] = 
    Parser(state =>
        pCond.run(state) match {
          case ParserSuccess(_, state1) => pTrue.run(state1)
          case ParserFailure(_) => pFalse.run(state) 
        }      
    )
    
  /*
  def pChoice3[A0 <: R, A1 <: R, A2 <: R, R](p0: Parser[A0], p1: Parser[A1], p2: Parser[A2]): Parser[R] =
    pChoice2(p0, pChoice2(p1, p2))
 
  def pChoice4[A0 <: R, A1 <: R, A2 <: R, A3 <: R, R](p0: Parser[A0], p1: Parser[A1], p2: Parser[A2],  p3: Parser[A3]): Parser[R] =
    pChoice2(p0, pChoice3(p1, p2, p3))    

  def pChoice5[A0 <: R, A1 <: R, A2 <: R, A3 <: R, A4 <: R, R](p0: Parser[A0], p1: Parser[A1], p2: Parser[A2], p3: Parser[A3], p4: Parser[A4]): Parser[R] =
    pChoice2(p0, pChoice4(p1, p2, p3, p4))   
   */
  def opt[A](p: Parser[A]): Parser[Option[A]] = Parser(state =>{
	  p.run(state) match{
	    case ParserSuccess(result, next) => ParserSuccess(Some(result), next)
	    case failure@ParserFailure(error) => ParserSuccess(None, state)
	  }
  })
  
  def pChoice[A](ps: Parser[A]*): Parser[A] = Parser(state => {
    //val state = state0.newState(state0.pos)
    val iter = ps.iterator
    
    var result: ParserResult[A] = failWith0(state.pos)
    var done = false
    
    while (!done && iter.hasNext){
      val p = iter.next()
      p.run(state) match {
        case success@ParserSuccess(_,_) => {done = true; result = success}
        case failure@ParserFailure(error) => {

        	  result = mergeFailures(result, failure)

        }
      } 
    }
    result match{
      case success@ParserSuccess(_,_) => success
      case failure@ParserFailure(ParserError(pos, msgs)) => ParserFailure(ParserError(pos, msgs.distinct))
    }
    //result
    })
  
  def pSetLastError[A](msg: String, p: Parser[A]): Parser[A] = Parser(state =>
  	p.run(state) match{
  	  case success@ParserSuccess(result, state) => success
      case ParserFailure(error) => ParserFailure(ParserError(error.pos, /*error.stack ++ */List(msg)))
  	})
  
  def pString(expected: String): Parser[String] = {
    val expectedArr = expected.toCharArray()
    Parser(state => 
      state.matchString(expectedArr) match {
          case Some(newState) => ParserSuccess(expected, newState)
          case None => failWith(state.pos, expected)
      }
    )
  }
  
  def pStringIgnore(expected: String) = pString(expected).bind(_=> pReturnEmpty)

  val pEmpty: Parser[Unit] = Parser(state => ParserSuccess(Unit, state))
  
  
  val pEOF: Parser[Unit] = Parser(state => 
      state.matchEOF match {
          case Some(newState) => ParserSuccess(Unit, newState)
          case None => failWith(state.pos, "end of file")
      }
    )
    
  def pRegexp(regexp: String): Parser[String] = {
    val pattern = regexp.r
    Parser(state => 
      state.matchRegex(pattern) match {
          case Some((newState, found)) => ParserSuccess(found, newState)
          case None => failWith(state.pos, "pRegexp(" + regexp + ")")
      }
    )
  }
  
  val pInt: Parser[Int] =
    Parser(state => 
      state.matchInt match {
          case Some((newState,value)) => ParserSuccess(value, newState)
          case None => failWith(state.pos, "pInt")
      })
 
  val pQuotedString: Parser[String] =
    Parser(state => 
      state.matchQuotedString match {
          case Some((newState,value)) => ParserSuccess(value, newState)
          case None => failWith(state.pos, "quoted string")
      })      
  
  val pWhitespaceAny = Parser(state =>
    ParserSuccess(Unit, state.skipWhiteSpace)
  )
  

  def map[A, B](p: Parser[A], f: A => B): Parser[B] = Parser(state =>
    p.run(state) match{
      case ParserSuccess(result, state) => ParserSuccess(f(result), state)
      case failure@ParserFailure(_) => failure
    })
    /*
  def chain[A, B] (p1: Parser[A], p2: Parser[B]): Parser[(A,B)] = Parser(state0 =>
    p1.run(state0) match {
      case ParserSuccess(result1, state1) => 
        p2.run(state1) match {
          case ParserSuccess(result2, state2) => ParserSuccess((result1, result2), state2)
          case failure@ParserFailure(_) => failure
        }
      case failure@ParserFailure(_) => failure
    })
*/
    
  def run[A](parser: Parser[A])(input: String): ParserResult[A] = {
    val state = ParserState(ArrayCharSequence.fromString(input), 0)
    parser.run(state)
    //failWith(0, "")
  }
}
