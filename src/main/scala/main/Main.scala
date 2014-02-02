package main

import parsers.Parsers
import parsers.ExpressionParser

object Test{
  
	case class MyInt(x : Int) {
		def addTo_:(i: Int): MyInt = MyInt(x+i)
	}

	val myInt = MyInt(4)
	4 addTo_: 3 addTo_: myInt



  case class Bind(s: Symbol, e: MyExpr){
	  //def &: (x: Bind): Bind = x //Bind(x, this)
  }
	
  

  class MyExpr{
    def =: (x: Symbol): Bind = Bind(x, this)
	def &: (x: Bind): MyExpr = BindingExpr(x, this) 
	def unary_! : Bind = Bind('ignore, this) //UnaryOp("!", thisExpr) 
	//def call_:() : Bind = Bind('ignore, this) //UnaryOp("!", thisExpr) 
	//def unary_do_ : Bind = Bind('ignore, this) //UnaryOp("!", thisExpr)
  }
  
  //case class Return(s: MyExpr) extends MyExpr
  case class Value(s: String) extends MyExpr
  case class Ident(s: Symbol) extends MyExpr
  case class BindingExpr(b: Bind, e: MyExpr) extends MyExpr
  case class Append(e1: MyExpr, e2: MyExpr) extends MyExpr
  case class If(cond: MyExpr, ifExpr: MyExpr, elseExpr: MyExpr) extends MyExpr
  case class Fun2(a: Symbol, b: Symbol, body: MyExpr) extends MyExpr
  case class Call2(e: Fun2, a: MyExpr, b: MyExpr) extends MyExpr
  
  case class DefFun2(a: Symbol, b: Symbol){
	  def -> (body: MyExpr) = Fun2(a,b,body)
  }
  //case class ExprWithBinding(map: Map[Symbol, String], )
  
  def evalHlp(bindings:Map[Symbol, String], expr: MyExpr): String = {
	  expr match {
	    case Value(s) => s
	    case Ident(s) => bindings(s)
	    case Append(e1,e2) => (evalHlp(bindings, e1) + evalHlp(bindings, e2))
	    case BindingExpr(Bind(s, value), rest) => {
	      val z = (s, evalHlp(bindings, value))
	      val binding1: Map[Symbol, String] = (bindings + z)
	      evalHlp(binding1, rest)
	    }
	  }
  }
  
  def eval(expr: MyExpr): String = {
    evalHlp(Map(), expr)
  }
  
  //def call2(fun2: Fun2, a: Symbol, b: Symbol) =  
  def test() = {
    val m = Map[Symbol, Int]()
    val q = m + ('m -> 1)

    val l = List()
    1::l
    val e = Value("hi") //.asInstanceOf[MyExpr]
    
    val z =  'x =: e
    val y = 'z =: e
    val k = z &:
    		y &: 
    		e
    
    //val hello = varName[String]("hello")
    //var 
    
    //def myappend(x: Symbol, y:Symbol) = 
    //Fun2(x, y, Append(Ident(x), Ident(y)))
    val myappend = {
    	val hello = 'hello
    	val world = 'world
    	DefFun2(hello, world) -> {
    		'a =: Value("hello") &:
    		Append(Ident(hello), Ident(world))
    	}
    }
    
    //
    // (sum_{w in terms}(tf[w]/(1.0 + doclen))) + 
    // doc = sentences*
    // sentence = phrase*
    // phrase=word*
    // 
    //
    //
    val e1 = Value("x")
    
    val __ = 'ignore
    val program = 'hello =: Value("hello") &: //;
    			  'world =: Value("world") &: //=: 
    			  __ =: If(e, 
    					{ 'test =: e &:
    					   e
    			  	    }, 
    			  	    e) &:
    			  __ =: Call2(myappend, Ident('hello), Ident('world)) &:
    			  //Append(Ident('hello), Ident('world)) &:
    			  Append(Ident('hello), Ident('world))
    			  
    """ja"""			  
    println(program)
    println(eval(program))
    ()
  }
  
}
object Main {
  def main(args: Array[String]) = {
    println("parsers")
    ExpressionParser.test2()
    //ExpressionParser.test3()
    ExpressionParser.testFile("""D:\Scala\test\testFile.myprog""")
    ExpressionParser.testFile("""D:\Scala\test\testFile2.myprog""")
    //Test.test()
  }
}
