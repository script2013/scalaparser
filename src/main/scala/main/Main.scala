package main

import parsers.Parsers
import parsers.ExpressionParser
import parsers.Evaluator



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
  
  class WrappedList extends java.util.ArrayList[Object]{}
  
  def testRef(){
    //getClass.getClassLoade
     val l = new java.util.ArrayList()
     val o = new Object()
     println(o.getClass().getName())
     println(Class.forName("java.lang.Object").getName())
     println(Class.forName("java.util.ArrayList").getName())
	 val clazz = l.getClass() // ClassLoader.getSystemClassLoader().loadClass("ArrayList");
	 val foo = clazz.newInstance()
	 val method1: java.lang.reflect.Method = clazz.getMethod("add", o.getClass());
	 method1.invoke(foo, new Integer(1));
		
	 println("finally: " + foo)

	  import scala.reflect.runtime.{universe => ru}
	  //import scala.reflect.runtime.universe.MethodSymbol
	  //import scala.reflect.runtime.universe.Name
	  //import scala.tools.E
	  val m = ru.runtimeMirror(getClass.getClassLoader)
	  
	  val typeList = ru.typeOf[WrappedList] //java.util.ArrayList[Object]]
	  //val ctor = listType.typeConstructor
	  val cm = m.reflectClass(typeList.typeSymbol.asClass)
	  //cm.
	  val ctor = typeList.declaration(ru.nme.CONSTRUCTOR).asMethod
	  val ctorm = cm.reflectConstructor(ctor)
	  val res = ctorm()
	 
	  //c.typeSignature.member("x").suchThat(_.isMethod)
	  
	  val tree = ru.Apply(ru.Select(ru.Ident("Macros"),
				ru.newTermName("foo")), List(ru.Literal(ru.Constant(42))))
	  //println(ru.Expr(expr).eval)
	  scala.reflect.runtime.universe.Expr
	  val e = ru.reify{ println("ok") }//val y = x + 2
	  //ru.Exprs.Expr(e).eval()
	  //ru.Expr(m, tree)
	  //println(e)
	  println("----------------")
	  ru.show(tree)
	  ru.showRaw(e, true, false, true, false)
	  println("----------------")
	  
	  
	  val methodDef = typeList.members.
	  				collect {case m: ru.MethodSymbol if !m.isPrivate && m.name == ru.newTermName("add")  => m -> m.typeSignatureIn(typeList)}. 
	  				collect {case (m, mt @ ru.MethodType(inputArgs, tpe)) if inputArgs.size == 1 => m} .
	  				head
	  val im = m.reflect(res)
	  val method = im.reflectMethod(methodDef)
	  method(new Integer(1))
	  println("Res:" + im)
	  
	  //http://dcsobral.blogspot.com.br/2012/07/json-serialization-with-reflection-in.html
	  //http://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html
	  val BoolType = ru.typeOf[Boolean]
	  val ObjType = ru.typeOf[Object]
      val vType   = ru.typeOf[java.util.List[Object]]
      
	  def isObj(s: ru.MethodType): Boolean = {
	   // s.asTerm == ObjType.typeSymbol
	    //scala.reflect.internal.Symbols.
	   
	  //  println(s.toString)
	    //s.as
	    //println(s.=:=(newMethodSy ru.newTermName("add")))
	    true
	  }
	  
	  
		val methods = vType.members.collect {
		case m: ru.MethodSymbol if !m.isPrivate => m -> m.typeSignatureIn(vType)
		}
		methods collect {
			//case (m, mt @ ru.NullaryMethodType(tpe))         /* if tpe =:= IntType*/ => m -> mt
			case (m, mt @ ru.MethodType(inputArgs, tpe))   if /* tpe =:= BoolType && */ inputArgs.size == 1 && isObj(mt) /*&& inputArgs(0) =:= ObjType*/ => m -> mt
			//case (m, mt @ ru.PolyType(_, ru.MethodType(_, tpe))) if tpe =:= BoolType => m -> mt
		} foreach{case (a,b) => 
		  	println( b.toString())
		  	}
/*
	   val methods = typeList.members.collect {
	   		//case m: MethodSymbol if !m.isPrivate && m.name == ru.newTermName("add") => m //-> m.typeSignatureIn(vType)
	   		case (m, mt @ PolyType(_, MethodType(_, tpe))) if tpe =:= IntType => m -> mt
	  } foreach(v => println( v.typeSignature))*/
  }
  def main(args: Array[String]) = {
    println("parsers")
    //testRef()
    
    /*
    ExpressionParser.test2()
    //ExpressionParser.test3()
    ExpressionParser.testFile("""D:\Scala\test\testFile.myprog""")
    ExpressionParser.testFile("""D:\Scala\test\testFile2.myprog""")*/
    val p = ExpressionParser.parseFile("""D:\Scala\test\testFile2.myprog""")
    println(Evaluator.evalProgram(p, "Main"))
    //Test.test()
    ()
  }
}
