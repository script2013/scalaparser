package parsers

object ExpressionParser{
  import Parsers._
  
  trait ParsedNode{
	  private[this] var loc: Option[Loc] = None
	  def setLoc(loc: Loc) = {this.loc = Some(loc)}
	  def getLoc: Loc = loc.get
  }
  
  case class Name(name: String) extends ParsedNode
  
  abstract class Pattern extends ParsedNode 
  case class SimpleIdent(val name: String) extends Pattern
  case class TuplePattern(lst: List[Pattern]) extends Pattern
  case class ClassPattern(val className: String, val lst: List[Pattern]) extends Pattern  
  
  abstract class Expr extends ParsedNode
  
  case class Number(val value: Int) extends Expr
  
  case class Bool(val value: Boolean) extends Expr
  case class StringExpr(val value: String) extends Expr
  
  //to do: Double
  case class Ident(val name: String) extends Expr
  case class PatternIdent(val pattern: Pattern) extends Expr
  
	object BinOp extends Enumeration with ParsedNode {
	  val Plus, Minus, Mult, Div, Mod, Lt, Gt, LtEq, GtEq, Eq, Neq  = Value
	  def fromString(name: String) = {
	    name match {
	      case "+" => Plus
	      case "-" => Minus
	      case "*" => Mult
	      case "/" => Div
	      case "%" => Mod
	      case "<" => Lt
	      case ">" => Gt
	      case "<=" => LtEq
	      case ">=" => GtEq
	      case "==" => Eq
	      case "!=" => Neq
	    }
	  }
	}
  
  case class BinOp(val name: BinOp.Value, val operand1: Expr, val operand2: Expr) extends Expr
  case class UnaryOp(val name: String, val operand: Expr) extends Expr
  case class TupleN(val operands: List[Expr]) extends Expr
  case class FunApp(val expr: Expr, val operands: List[Expr]) extends Expr
  case class ArrayAccess(val expr: Expr, val operands: List[Expr]) extends Expr
  case class PropAccess(val expr: Expr, val name: String) extends Expr
  case class AnonFun(val paramNames: List[String], val body: Expr) extends Expr
  case class ExprSeq(val exprs: List[Expr]) extends Expr
  case class Bind(val pattern: Pattern, val isRef: Boolean, val operand: Expr) extends Expr
  case class IfThenExpr(val cond: Expr, val ifStmt: Expr) extends Expr
  case class IfThenElseExpr(val cond: Expr, val ifStmt: Expr, val elseStmt: Expr) extends Expr  
  case class WhileExpr(val cond: Expr, val body: Expr) extends Expr 
  case class ListInit(val values: List[Expr]) extends Expr
  case class AssgnmentStmt(val lhs: Expr, val rhs: Expr) extends Expr
  
  //to do: introduce definitions
  abstract class Def extends ParsedNode{
    def getName: String
  }
  
  case class ClassDef(val ident: Name, args: List[String]) extends Def{
	   def getName = ident.name //.name
  }
  case class FunDef(val ident: Name, args: List[String], body: Expr) extends Def{
	   def getName = ident.name //.name
  } 
  case class ModuleDef(val ident: Name, defs: List[Def]) extends Def{
       def getName = ident.name //.name
  }
  
  case class Program(val defs: List[Def]) extends Def{
	  def getName = "<program>"
  }
  
  
  object Implicits{
	  val noLoc = Loc(0, 0)
	  
	  class ExprWithOp[A <: Expr](thisExpr: A) {
	    def +[B <: Expr](that: B): Expr = BinOp(BinOp.Plus, thisExpr, that)  
	    def -(that: Expr): Expr = BinOp(BinOp.Minus, thisExpr, that) 
	    def *(that: Expr): Expr = BinOp(BinOp.Mult, thisExpr, that)  
	    def /(that: Expr): Expr = BinOp(BinOp.Div, thisExpr, that)  
	   	def %(that: Expr): Expr = BinOp(BinOp.Mod, thisExpr, that)   
	   	def <(that: Expr): Expr = BinOp(BinOp.Lt, thisExpr, that)
	   	def >(that: Expr): Expr = BinOp(BinOp.Gt, thisExpr, that)  
	   	def <= (that: Expr): Expr = BinOp(BinOp.LtEq, thisExpr, that)  	 
	   	def >= (that: Expr): Expr = BinOp(BinOp.GtEq, thisExpr, that)  	 
	   	def === (that: Expr): Expr = BinOp(BinOp.Eq, thisExpr, that)  	 
	   	def !== (that: Expr): Expr = BinOp(BinOp.Neq, thisExpr, that)  	  
	   	
	   	def unary_+ : Expr = UnaryOp("+", thisExpr)   	
	   	def unary_- : Expr = UnaryOp("-", thisExpr) 
	   	def unary_! : Expr = UnaryOp("!", thisExpr) 
	   	
	    def apply[B <: Expr](args: Expr*) = FunApp(thisExpr,args.toList)
	    def at(that: Expr): Expr = ArrayAccess(thisExpr, List(that))  
	  }

	  implicit class IdentWithOp(ident: Ident){
	    def ^=^(that: Expr): Expr = Bind(SimpleIdent(ident.name), false, that)
	  }

	  implicit def toExprWithOp[A <: Expr](expr: A) = new ExprWithOp(expr)  
  }
  
  def setLoc[A <: ParsedNode](p: Parser[A]): Parser[A] = 
  pWithLocation(p, {
    (res: A, loc) => {res.setLoc(loc); res} 
  } )
      
  object withLoc{
    def <| [A <: ParsedNode](p: Parser[A]): Parser[A] = setLoc(p)
  }
  
  //Bind(x,value)
  def exprParser(): (Parser[Expr], Parser[Program]) = {
    //see http://introcs.cs.princeton.edu/java/11precedence/ for operator precedence

    
    val (pExprRef, pExpr) = pRef[Expr]() //forward declaration of expr parser
    
    val pIdentName = pSetLastError("identifier", pRegexp("""[a-zA-Z][a-zA-Z0-9]*"""))
    
    val pIdentName2 = withLoc <| pIdentName.map(name => Name(name))
        
    val (kwFun, kwIf, kwWhile, kwModule, kwClass, kwLet, kwVar) = ("fun", "if", "while", "module", "class", "let", "var")
    val (kwTrue, kwFalse) = ("true", "false")
    
    val keywords = Set(kwFun, kwIf, kwWhile, kwModule, kwClass, kwTrue, kwFalse)
    
    val pIdent: Parser[Ident] = 
      		setLoc(pIdentName.bind(ident => 
      						   if (keywords.contains(ident)) pFail("not a valid ident") //to do: pFailWithLoc 
      						   else pReturn(Ident(ident))) )
      						   
    def pKeyword(kw: String) = pRegexp("""[a-zA-Z]+""").bind(name => if (name == kw) pReturnEmpty else pFail("keyword " + kw))
    
    val pComma: Parser[Unit] = pString(",").ignore // .bind(info => pReturnEmpty(info.loc))
    val pOpenBracket = pString("(")
    val pCloseBracket = pString(")")
    
    def pBracketedListWithSep0[A](openBracket: Parser[Unit], closeBracket: Parser[Unit], pSep: Parser[Unit], expr: Parser[A]): Parser[List[A]] = 
	  openBracket.bind(_ =>
      pWhile (not(closeBracket)) {
    	  	   expr.bind(ident => 
			   pChoice(lookAhead(closeBracket), pSep).bind(_=>
			   pReturn(ident)))  }.bind(lst =>
	  closeBracket.bind(_ =>
	  pReturn(lst))))
	  
	  
    def pBracketedListWithSep[A](openBracket: Parser[Unit], closeBracket: Parser[Unit], pSep: Parser[Unit], expr: Parser[A]): Parser[List[A]] = for{
        _ <- openBracket;
        lst <- pWhile (not(closeBracket)) {for{ ident <- expr;
        										 _ <- pChoice(lookAhead(closeBracket), pSep)}
        									yield ident};
		_ <- closeBracket
      }
      yield lst
	  	  
	//to do:
    def pBracketedListWithSepNoOpenBracket[A](closeBracket: Parser[Unit], pSep: Parser[Unit], expr: Parser[A]): Parser[List[A]] = 
      pWhile (not(closeBracket)) (expr.bind(ident => 
				      				pChoice(lookAhead(closeBracket), pSep).bind(_=>
				      				pReturn(ident)))).bind(lst =>
				      			closeBracket.bind(_ =>
				      			pReturn(lst)))    
				      			
	/*
	 * 
	do parse("(")
	val res = 
		whileNot ")" {
			val exprRes = expr
			do (parse(",") | dontConsume(parse(")")))
			return exprRes
		}
	do parse(")")
	return expr
	 */

    def roundBracketCommaSep[A](pItem: Parser[A]): Parser[List[A]] = 
      	pBracketedListWithSep(pStringIgnore("("), pStringIgnore(")"), pStringIgnore(","), pItem)
      	
    val pArgsDef = roundBracketCommaSep(pIdentName) 
     
   // def pReturnExpr[A <: Expr](expr: A):Parser[A] = pReturn(expr, expr.loc)
   // def pReturnDef[A <: Def](adef: A):Parser[A] = pReturn(adef, adef.loc)
    
    //class X(a,b,c){
    val pClassDef: Parser[Def] = withLoc <|
      				pKeyword("class").bind(_ => 
    				pIdentName2.bind(name => 
    				pArgsDef.bind(args =>
    				pReturn(ClassDef(name, args))))) 
    
    val pNamedFun: Parser[Def] = withLoc <|
      				pKeyword("fun").bind(fun =>
    				pIdentName2.bind(ident => 
    				pArgsDef.bind(args =>      
    				pString("=").bind(_ =>
    				pExpr.bind(expr =>
    				pReturn(FunDef(ident, args, expr)))))))  

    val (pModDefRef, pModuleDef) = pRef[Def]()
    
    val x = pChoice(pModuleDef, pClassDef, pNamedFun) // pModuleDef)
    
	val pResolvedModuleDef =  withLoc <|
     				pKeyword("module").bind(mod =>
      				 pIdentName2.bind(name =>
      				 pString("{").bind(_ =>
      				 pWhile (not(pString("}"))) (x)).bind(defs =>
      				 pString("}").bind(brk =>
      				 pReturn(ModuleDef(name, defs))))))
      				 
    pModDefRef.set(pResolvedModuleDef)

    val pProgram = (pWhile(not(pEOF)) (pResolvedModuleDef) ).bind(modDefs => pReturn(Program(modDefs))) //fix loc
    
    def anonFunc =
      pKeyword("fun").bind(_ =>
      pArgsDef.bind(args => 
      pString("->").bind(_ =>
      pExpr.bind(expr =>
      pReturn(AnonFun(args, expr))))))

  
    val pNumber: Parser[Expr] = pSetLastError("int", pInt.bind(num => pReturn(Number(num))))

    val pListInitialization: Parser[Expr] = pBracketedListWithSep(pStringIgnore("["), pStringIgnore("]"), pStringIgnore(","), pExpr).bind(lst =>
    										pReturn(ListInit(lst)))

    val pUnaryOpName: Parser[String] = pChoice(pString("+"), pString("-"), pString("!")) //, pString("~"))
    
    val pUnaryOp: Parser[Expr] = 
        pUnaryOpName.bind(unaryOpName =>
        pExpr.bind(expr =>
        pReturn(UnaryOp(unaryOpName,expr))))

    val bracketedExprOrTuple =
      roundBracketCommaSep(pExpr).bind(exprs => {
      val expr = if (exprs.length == 1) exprs(0) else TupleN(exprs)
      pReturn(expr)})
    
    //val x1 = ident
    
    val (pPatternRef, pPattern) = pRef[Pattern]() 
    val pListOfPat = pBracketedListWithSep(pStringIgnore("("), pStringIgnore(")"), pStringIgnore(","), pPattern)
    val pSimpleIdentOrClass = withLoc <|
      		pIdentName.bind(name =>
      						  pIf(lookAhead(pStringIgnore("(")), pListOfPat.bind(lst => pReturn(ClassPattern(name, lst))),
      						     pReturn(SimpleIdent(name))))
      						     
    val pTupleRef = pListOfPat.bind(lst => pReturn(TuplePattern(lst))) 	
    
    val resolvedPat = pIf(lookAhead(pStringIgnore("(")), pTupleRef, pSimpleIdentOrClass)
    pPatternRef.set(resolvedPat)
    
    val pExprOrAssignment = pExpr.bind(expr =>
      						pIf(pStringIgnore(":="), pExpr.bind(rhs => pReturn(AssgnmentStmt(expr, rhs))),
      						pReturn(expr)))
 
    val letOrVar = pChoice(pKeyword(kwLet).bind(_=>pReturn(false)), pKeyword(kwVar).bind(_=> pReturn(true)))
    
    val letExpr = 
      pIf(lookAhead(letOrVar),
	          {	
    	  		letOrVar.bind(isRef => 
    	  		pPattern.bind(name => //pIdentName.bind(name => 
	          	pString("=").bind(_ => 
	          	pExpr.bind(expr => 
	          	pReturn(Bind(name, isRef, expr))))))},
	          //else
	          pExprOrAssignment)
          
  
    val l2 = (letExpr.bind(e => opt(pString(";")).bind(_=>pReturn(e))))
    val pExprSeq: Parser[Expr] = 
      pIf(pString("{"), 
           { (pWhile(not(pString("}"))) (l2)).bind(lst =>
             pString("}").bind(_ =>
             pReturn(ExprSeq(lst)))) },
          //else
          bracketedExprOrTuple)          
          
    val pWhileExpr: Parser[Expr] =
      pKeyword("while").bind(_ =>
      pString("(").bind(_ =>
      pExpr.bind(cond=>
      pString(")").bind(_ => 
      pExpr.bind(body => 
      pReturn(WhileExpr(cond,body)))))))
    
    val pIfExpr1: Parser[Expr] =
      pKeyword("if").bind(_ =>
      pString("(").bind(_ =>
      pExpr.bind(cond=>
      pString(")").bind(_ => 
      pExpr.bind(ifExpr => 
      pIf(pString("else"), 
          pExpr.bind(elseExpr => pReturn(IfThenElseExpr(cond,ifExpr,elseExpr))), 
          pEmpty.bind(_=> pReturn(IfThenExpr(cond,ifExpr)))))))))
    
    val pIfExpr: Parser[Expr] = for{
      _ <- pKeyword("if");
      _ <- pString("(");
      cond <- pExpr;
      _ <- pString(")");
      ifExpr <- pExpr;
      resExpr <- pIf(pString("else"), 
    		  		 for{ elseExpr <- pExpr } yield IfThenElseExpr(cond, ifExpr, elseExpr),
    		  		 for{ _ <- pEmpty } yield IfThenExpr(cond, ifExpr))
    } yield resExpr
      
    val pQuotedString = Parsers.pQuotedString.bind(v => 
      					pReturn(StringExpr(v)))
    val pTrueFalse = pChoice(pKeyword(kwTrue).bind(_ => pReturn(Bool(true))),
    						 pKeyword(kwFalse).bind(_ => pReturn(Bool(false))))
    //println(1 :: List(2, 3, 4))  
      
    val pFactor: Parser[Expr] = pChoice(anonFunc, pIfExpr, pWhileExpr, pTrueFalse, pIdent, pNumber, pQuotedString, pUnaryOp, pListInitialization, pExprSeq) //bracketedExprOrTuple))
    //def pFactor: Parser[Expr] = pExpon(pFactor0) // pChoice5(anonFunc, pFunAppOrIdent, pNumber, pUnaryOp, bracketedExprOrTuple)
    
      /*
      pBaseParser.bind(expr0 => 
      (pWhile (lookAhead(pOpParser)) (pOpParser.bind(opName =>
      	pBaseParser.bind(expr2 =>
      	pReturn((opName, expr2)))))).bind(lst =>
        pReturn(lst.foldLeft(expr0){case (f0, (op, f1)) =>  BinOp(op,f0, f1) }) 
       ))*/    
    def pLeftAssoc(pBaseParser: Parser[Expr], pOpParser: Parser[String]): Parser[Expr] = for{
        expr0 <- pBaseParser;
        lst <- pWhile (lookAhead(pOpParser)) (for{
				          opName <- pOpParser;
				          expr2 <- pBaseParser
				       } yield (opName, expr2))
      }
      yield lst.foldLeft(expr0) {case (f0, (op, f1)) =>  BinOp(BinOp.fromString(op), f0, f1)}

    def pRecFunApp: Parser[Expr] = {
      val pOpenBracket = pChoice(pString("("), pString("["), pString("."))
      val closingBracketParsers = Map("(" -> pStringIgnore(")"), 
    		  					      "[" -> pStringIgnore("]"))
      
      val getExprComb = Map("(" -> FunApp.apply _, "[" -> ArrayAccess.apply _)
      //to do: will wrongly accept [], but () is ok
      def getFunAppArgs(opBracket: String) =
        	pBracketedListWithSepNoOpenBracket(closingBracketParsers(opBracket), pStringIgnore(","), pExpr)
     
      pFactor.bind(baseResult => {
        var expr: Expr = baseResult
        pWhile(lookAhead(pOpenBracket)) (pIf(pString("."), pIdentName.bind(name => {expr = PropAccess(expr, name); pReturn(())}),
        		    pOpenBracket.bind(opName => 
	                getFunAppArgs(opName).bind(expr2 =>{
	                expr = getExprComb(opName)(expr, expr2) //BinOp(opName, expr, expr2)
	                pReturn(())})))).bind(res =>
	        pReturn(expr))})
    }
    
   
    val pMultDiv = pChoice(pString("*"), pString("/"), pString("%")) //precedence 4
    val pPlusMinus = pChoice(pString("+"), pString("-")) //precedence 5
    val pCmp = pChoice(pString("<="), pString(">="), pString("<"), pString(">")) //prec 7
    val pCmpEq = pChoice(pString("=="), pString("!="))
    
    
    val pResolvedExpr = pLeftAssoc(pLeftAssoc(pLeftAssoc(pLeftAssoc(pRecFunApp, pMultDiv), pPlusMinus), pCmp), pCmpEq)
    
   
    //leftAssoc(leftAssoc(leftAssoc(..., funApp), 
    pExprRef.set(pResolvedExpr)
    
    val pFullExpr = pExpr.bind(expr => pEOF.bind(_ => pReturn(expr)))
    val pFullProgram = pProgram.bind(prog => pEOF.bind(_ => pReturn(prog)))
    (pFullExpr, pFullProgram)
  }
  
  object TestFragments{
    val (a,b,c,d) = (Ident("a"), Ident("b"), Ident("c"), Ident("d"))
    val (f, g, h) = (Ident("f"), Ident("g"), Ident("h"))
    val (_0, _1, _2) = (Number(0), Number(1), Number(2))
    def tuple2[A <: Expr, B <: Expr](a: A, b: B) = TupleN(List(a, b))
    def tuple3[A <: Expr, B <: Expr, C <: Expr](a: A, b: B, c: C) = TupleN(List(a, b, c))
    def fun0[A <: Expr](map: (Unit, A)) = {val body = map._2; AnonFun(List(),body)} 
    def fun1[A <: Expr](map: (Ident, A)) = {val arg1 = map._1.name; val body = map._2; AnonFun(List(arg1),body)}
    def fun2[A <: Expr](map: ((Ident, Ident), A)) = {
    	val arg1 = map._1._1.name; 
    	val arg2 = map._1._2.name; 
    	val body = map._2; 
    	AnonFun(List(arg1, arg2),body)}     
    
    def block[A <: Expr](s: A*) = ExprSeq(s.toList)    
  }
  
  def test3() = {
    import TestFragments._
    
    val (pExpr,pProgram) = exprParser()
    val input0 = """| a 
    			    | +""".stripMargin
    
    val input1 = "(a + (b + t)*fun () -> a "
    val input = "b   ! "
    val error = Parsers.run(pExpr)(input).failure
    println(error)
    error.printMessage(input)
    
  }
  
  def parseFile(fileName: String): Program = {
    val contents = scala.io.Source.fromFile(fileName).getLines().mkString("\n").replace("\t", "  ")
    println(contents)
    val (pExpr,pProgram) = exprParser()
    Parsers.run(pProgram)(contents) match {
      case ParserSuccess(result,_) => {println("Success"); println(result); result}
      case ParserFailure(error) =>{
    	  error.printMessage(contents);
    	  throw new Exception("cannot parse")
      }
    }
  }
  
  def test2() = {
    import Implicits._
    
    val (a,b,c,d) = (Ident("a"), Ident("b"), Ident("c"), Ident("d"))
    val (_true, _false) = (Bool(true), Bool(false))
    val (f, g, h) = (Ident("f"), Ident("g"), Ident("h"))
    val (_0, _1, _2) = (Number(0), Number(1), Number(2))
    def tuple2[A <: Expr, B <: Expr](a: A, b: B) = TupleN(List(a, b))
    def tuple3[A <: Expr, B <: Expr, C <: Expr](a: A, b: B, c: C) = TupleN(List(a, b, c))
    def fun0[A <: Expr](map: (Unit, A)) = {val body = map._2; AnonFun(List(),body)} 
    def fun1[A <: Expr](map: (Ident, A)) = {val arg1 = map._1.name; val body = map._2; AnonFun(List(arg1),body)}
    def fun2[A <: Expr](map: ((Ident, Ident), A)) = {
    	val arg1 = map._1._1.name; 
    	val arg2 = map._1._2.name; 
    	val body = map._2; 
    	AnonFun(List(arg1, arg2),body)}     
    
    def block[A <: Expr](s: A*) = ExprSeq(s.toList)
    
    //def f[A <: Expr](args: A*) = FunApp(Ident("f"), )  
    val testCases: List[(String, Expr)] = List(
			    		"a" -> a,
			    		"1" -> _1,
			    		""""a quoted string"""" -> StringExpr("a quoted string"),
			    		""""a quoted\" string"""" -> StringExpr("a quoted\" string"),
			    		""""a quoted\n string"""" -> StringExpr("a quoted\n string"),
			    		"true" -> _true,
			    		"false" -> _false,
			    		"a + b /*a comment \n multiline*/" -> (a + b),
			    		"1 + b" -> (_1 + b),
			    		"a + b + c" -> (a + b + c),
			    		"(a + b) + c" -> ((a + b) + c),
			    		"(a + (b + c))" -> (a + (b + c)),
			    		"a * b + c" -> (a * b + c),
			    		"a + b / c" -> (a + b / c),
			    		"(a,b)" -> tuple2(a,b),
			    		"(a,b,c)" -> tuple3(a,b,c),
			    		"(a,(b,c),d)" -> tuple3(a,tuple2(b,c), d),
			    		"f()" -> f(), 
			    		"f(a)" -> f(a), 
			    		"f(a, b)" -> f(a, b), 
			    		"f((a,b))" -> f(tuple2(a, b)),
			    		"f(a + b, c)" -> f(a + b, c),
			    		"f[0]" -> (f at _0),
			    		"(a + b)(c)" -> ((a + b)(c)),
			    		"(a + b)(c)[d]" -> (((a + b)(c)) at d),
			    		"a < b" -> (a < b),
			    		"a > b" -> (a > b),
			    		"a <= b" -> (a <= b),
			    		"a >= b" -> (a >= b),
			    		"a == b" -> (a === b),
			    		"a != b" -> (a !== b),
			    		"fun () -> 0" -> (fun0(() -> _0)),
			    		"fun () -> (a + b + c)" -> fun0{() -> (a + b + c)},
			    		"fun (a) -> a + 1" -> fun1{a -> (a + _1)},
			    		"(fun (a) -> fun (b) -> a + b)(0)(1)" -> ((fun1{a -> (fun1{b -> (a + b)})})(_0)(_1)),
			    		"fun () -> fun() -> a" -> fun0{() -> fun0{() -> a}},
			    		"fun (a,b) -> a + b" -> fun2{(a,b) -> (a + b)},
			    		"fun (a,b) -> f(a,b)" -> fun2{(a,b) -> (f(a, b))},
			    		"- 1" -> (- _1),
			    		"-- 1" -> (- (- _1)),
			    		"+ + 1" -> (+ (+ _1)),
			    		"a + + + 1" -> (a + (+ (+ _1))),
			    		"a+++1" -> (a + (+ (+ _1))),
			    		"a+ ! ! 1" -> (a + (! (! _1))),
			    		"{a;}" -> block(a),
			    		"{a;b;}" -> block(a,b),
			    		"{a + b; 1 + 2;}" -> block(a + b, _1 + _2),
			    		"{let c = 1;}" -> block(c ^=^ _1),
			    		"{let a = b + c; let d = a + c;}" -> block(a ^=^ (b + c), d ^=^ (a + c)),
			    		"""{let a = {let b = 1 + 2; c + b;}; a + 1;}""" -> 
															    				block(a ^=^ (block(b ^=^ _1 + _2,
															    				 			  c + b)),
															    				    a + _1),
						"if (1 == 1) 2" -> IfThenExpr(_1 === _1, _2),
						"if (1) 2 else 0" -> IfThenElseExpr(_1, _2, _0),
						"if (a) if (0) 1 else c else b" -> IfThenElseExpr(a, IfThenElseExpr(_0, _1, c), b),
						"if (a) 1 else if (b) 2 else c" -> IfThenElseExpr(a, _1, IfThenElseExpr(b, _2, c)),
						"if (a) if (0) 1 else c" -> IfThenExpr(a, IfThenElseExpr(_0, _1, c)),
						"while (a) (a + b)" -> WhileExpr(a, a + b),
			    		"b" -> b
    )
    val x = !(!(!false))
    val (pExpr,pProgram) = exprParser()

    for((input, expected) <- testCases){
    	val res = Parsers.run(pExpr)(input)
    	if (!res.isSuccess){
    	  println(res.failure.printMessage(input))
    	}
    	val expr = res.success
    	if (expected != expr){
    	  println("Failed (Result): " + expr)
    	  println("Expected       : " + expected)
    	  throw new Exception("Test failed for input " + input)
    	}
    	println("Success: " + input + " : " + expr)
    }
    
    val p1 = Program(List(ModuleDef(Name("X"), List(ClassDef(Name("Y"), List("a", "b", "c")))))) 
    val p2 = Program(List(ModuleDef(Name("X"), List(FunDef(Name("Y"), List("a", "b", "c"), _0)))))
    val funDefY = FunDef(Name("Y"), List("a", "b", "c"), a + b + c)
    val funDefZ = FunDef(Name("Z"), List(), _0)
    val classM = ClassDef(Name("M"), List("a", "b", "c"))
    val p3 = Program(List(ModuleDef(Name("X"), List(funDefY, funDefZ, classM))))    
    
    val modZ = ModuleDef(Name("Z"), List(FunDef(Name("K"), List(), _1 + b)))
    val p4 = Program(List(ModuleDef(Name("X"), List(funDefY, funDefZ, classM, modZ))))    
    
    val testCasesDef: List[(String, Program)] = List( 
        "module X{}" -> Program(List(ModuleDef(Name("X"), List()))),
     """|module X{
        |	class Y(a,b,c)	
  		|}""".stripMargin -> p1,
  	 """|module X{
  		|	fun Y(a,b,c) = 0
  		|}""".stripMargin -> p2,
  	 """|module X{
  		|	fun Y(a,b,c) = a + b + c
  		| 	fun Z() = 0
  		|   class M(a,b,c)
  		|}""".stripMargin -> p3,  		

  	 """|module X{
  		|	fun Y(a,b,c) = a + b + c
  		| 	fun Z() = 0
  		|   class M(a,b,c)
  		| 	module Z{
  		|		fun K() = 1 + b	
  		|	}
  		|}""".stripMargin -> p4      
    )
    //interface Y{
    //	fun Y(a,b,c)
    //	fun Z()
    // 	fun M()
    //}
    //interface K{ }
    //class Z(m:Int) implements Y + K
    //(merges both name)
    //fun f(a:'a,b:'b) 
    //	where 'a: {fun plus();fun k()}
    //   and  'b: L + B + Z //union of all functions in the interfaces
    //   and  'c: L | M | N --not sure 
    //generics in functions, but not in classes (?)
    //class(x: string, y: int, k: list<(int,int)>)
    //set(list(values))
    //
    for((input, expected) <- testCasesDef){
    	val prog: Program = Parsers.run(pProgram)(input).success
    	if (expected != prog){
    	  println("Failed (Result): " + prog)
    	  println("Expected       : " + expected)
    	  throw new Exception("Test failed for input " + input)
    	}
    	println("Success: " + input + " : " + prog)
    }
    println("done")
  }
  
  def test() {
    val s = ParserState.init("input")
    println("regex" + s.matchRegex("input".r))
    
    val p1 = pString("inp")
    val p2 = pString("ut")
    val p3 = p1 // p1 ==> p2 ==> p1 //chain(p1,p2)
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
    val pOp = pChoice(pPlus, pMinus)
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
  
  
}