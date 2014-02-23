package parsers

import sun.org.mozilla.javascript.internal.ast.Assignment


object Evaluator{
  import ExpressionParser._
  import scala.collection._
  
  abstract class RuntimeValue{
    def toObject: Object
  }
  
  trait RTComparable[A]{ self: A =>
	  def compareTo(other: A): Int
  }
  
  case class RTClassDef(name: String, names: List[String]) extends RuntimeValue{
    def toObject: Object = error("not implemented")
  }
  
  case class RTImportedClassDef(fullyQualifiedName: String) extends RuntimeValue{
    def toObject: Object = error("not implemented")
  }
  
  case class RTUnit extends RuntimeValue{
    def toObject: Object = null
  }
  
  case class RTString(value: String) extends RuntimeValue with RTComparable[RTString]{
    def compareTo(other:RTString) = {
    	this.value.compareTo(other.value)
    }
    def toObject: Object = value
  }
  
  case class RTInt(value: Int) extends RuntimeValue with RTComparable[RTInt]{
    def compareTo(other:RTInt) = {
    	this.value.compareTo(other.value)
    }    
    def toObject: Object = new Integer(value)
  }
  
  case class RTBool(value: Boolean) extends RuntimeValue with RTComparable[RTBool]{
    def compareTo(other:RTBool) = {
    	this.value.compareTo(other.value)
    }     
    def toObject: Object = new java.lang.Boolean(value)
  }  
  
  case class RTObject(clazz: RTClassDef, instanceVars: List[RuntimeValue]) extends RuntimeValue{
    def toObject: Object = error("not implemented")
  }
  
  case class RTImportedObject(clazz: java.lang.Class[_], obj: java.lang.Object) extends RuntimeValue{
    def toObject: Object = obj //error("not implemented")
  }
  
  case class RTImportedFun(obj: Object, methodName: String) extends RuntimeValue{
    def toObject: Object = obj //error("not implemented")
  }
  
  case class RTList(values: java.util.List[RuntimeValue]) extends RuntimeValue{
	  def toObject: Object = values
  }
  
  case class RTTuple(fields: List[RuntimeValue]) extends RuntimeValue{
    def toObject: Object = error("not implemented")
  }
  
  case class RTFun(argNames: List[String], body: Expr) extends RuntimeValue{
    def toObject: Object = error("not implemented")
  } //to do: must include env
  
  case class RTRef(var value: RuntimeValue) extends RuntimeValue{
    def toObject: Object = error("not implemented")
  }
  
  def buildMap(defList: List[Def]): immutable.Map[String, Def] = {
	  val name2Def = mutable.Map[String, Def]()
	  for(objectDef <- defList){
	    val name = objectDef.getName
	    if (name2Def.contains(name)){
	      val loc = objectDef.getLoc
	      throw new Exception("Name " + name + " " + loc + " exists twice")
	    }
	    name2Def.put(name, objectDef)
	  } 
	  name2Def.toMap
  }
  
  def verifyUniqueNames(objectName: String, lst: List[String]) = {
	  val names = mutable.Set[String]()
	  for(name <- lst){
	    if (names.contains(name)){
	      throw new Exception("Name " + name + " is not uqique in " + name)
	    }
	    names += name
	  }     
  }
  
  def extendEnvironment(env: immutable.Map[String, RuntimeValue], names: List[String], values: List[RuntimeValue]): immutable.Map[String, RuntimeValue] = {
	  //will override the old values
	  var newEnv = env
	  for(i <- 0 until names.length){
		  newEnv += ((names(i), values(i)))
	  }
	  newEnv
  }
  type Env = immutable.Map[String, RuntimeValue]
  
  object Env{
    def getValue(env: Env, name: String, rhs: Boolean): RuntimeValue = {
      val value = env.get(name).get
      if (value != null && rhs && value.isInstanceOf[RTRef]){
        value.asInstanceOf[RTRef].value
      }
      else{
        value
      }
    }    
  }
  
  def asIntValue(v1: RuntimeValue): Int = {
    v1.asInstanceOf[RTInt].value
  }
  
  def asBoolValue(v1: RuntimeValue): Boolean = {
    v1.asInstanceOf[RTBool].value
  }  
  
  def asFun(v1: RuntimeValue): RTFun = {
    v1.asInstanceOf[RTFun]
  }  
  
  def asRTList(v1: RuntimeValue): RTList = {
    v1.asInstanceOf[RTList]
  }    
  
  def asRTRef(v1: RuntimeValue): RTRef = {
    v1.asInstanceOf[RTRef]
  }    
  
  def binOpOnInts(binOp: (Int, Int) => Int, v1: RuntimeValue, v2: => RuntimeValue): RuntimeValue = {
    val intValue1 = asIntValue(v1)
    val intValue2 = asIntValue(v2)
    RTInt(binOp(intValue1,intValue2))
  }
  
  def unaryOpInt(unaryOp: Int => Int, v1: RuntimeValue): RuntimeValue = {
    val intValue1 = asIntValue(v1)
    RTInt(unaryOp(intValue1))
  }
  
  def unaryOpBool(unaryOp: Boolean => Boolean, v1: RuntimeValue): RuntimeValue = {
    val boolValue = asBoolValue(v1)
    RTBool(unaryOp(boolValue))
  }  
  
  /*
  def evalEq(rt1: RuntimeValue, rt2: RuntimeValue): Boolean = {
		  (rt1, rt2) match {
		    case (RTInt(v1), RTInt(v2)) => v1 == v2
		    case (RTBool(v1), RTBool(v2)) => v1 == v2
		    case (RTString(v1), RTString(v2)) => v1 == v2
		  }
  }
  */
  def evalCmp(resToBool: Int => Boolean, rt1: RuntimeValue, rt2: RuntimeValue): Boolean = {
	 val res = 
		 (rt1, rt2) match {
		    case (v1@RTInt(_), v2@RTInt(_)) => v1.compareTo(v2)
		    case (v1@RTBool(_), v2@RTBool(_)) => v1.compareTo(v2)
		    case (v1@RTString(_), v2@RTString(_)) => v1.compareTo(v2)
		  }
	 resToBool(res)
  }  
  
  def evalBinOp(binOp: BinOp, env: Env): RuntimeValue = {
	  binOp.name match{
	    case BinOp.Plus => binOpOnInts( (x, y) => x + y, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env))
	    case BinOp.Minus => binOpOnInts( (x, y) => x - y, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env))
	    case BinOp.Div => binOpOnInts( (x, y) => x / y, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env))	
	    case BinOp.Mult => binOpOnInts( (x, y) => x * y, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env))	
	    case BinOp.Eq => RTBool(evalCmp(r => r == 0, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env)))
	    case BinOp.Lt => RTBool(evalCmp(r => r < 0, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env)))
	    case BinOp.LtEq => RTBool(evalCmp(r => r <= 0, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env)))
	    case BinOp.Gt => RTBool(evalCmp(r => r > 0, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env)))
	    case BinOp.GtEq => RTBool(evalCmp(r => r >= 0, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env)))
	    case BinOp.Neq => RTBool(evalCmp(r => r != 0, evalExpr(binOp.operand1, env), evalExpr(binOp.operand2, env)))
	    case _ => throw new Exception("evalBinOp")
	  }
  }
  
  def evalUnaryOp(unaryOp: UnaryOp, env: Env): RuntimeValue = {
	  unaryOp.name match{
	    case "+" => unaryOpInt(x => x, evalExpr(unaryOp.operand, env))
	    case "-" => unaryOpInt(x => -x, evalExpr(unaryOp.operand, env))
	    case "!" => unaryOpBool(x => !x, evalExpr(unaryOp.operand, env))
	    case _ => throw new Exception("unaryOp")
	  }
  }
  
  def evalBlockOfExpr(lst: List[Expr], env: Env): RuntimeValue = {
		var result: RuntimeValue = RTUnit()
		var newEnv = env
		for(expr <- lst){
			val (nextRes, nextEnv) = evalWithNewEnv(expr, newEnv)
			result = nextRes
			newEnv = nextEnv
		}
		  
		result
  }
  
  def patternMatchList(patList: List[Pattern], values: List[RuntimeValue]) = {
		  var lst = List[(String, RuntimeValue)]()
		  var i = 0
		  while(i < patList.length){
		    val pat = patList(i)
		    val rtValue = values(i)
		    lst = lst ++ patternMatch(pat, rtValue)
		    i += 1
		  }
		  lst
  }
  
  def patternMatch(pat: Pattern, res: RuntimeValue): List[(String,RuntimeValue)] = {
    
    pat match{
      case SimpleIdent(name) => {println("" + pat.getLoc + " " + name); List((name, res))}
      case TuplePattern(lst) => {
        res match{
          case RTTuple(fields) => {
            if (lst.length != fields.length) throw new Exception("length mismatch")
            patternMatchList(lst, fields)
          }
          case _ => throw new Exception("expected tuple")
        }
      }
      case ClassPattern(className, lst: List[Pattern]) => {
        res match {
          case RTObject(_, fields) => {
            if (lst.length != fields.length) throw new Exception("length mismatch")
            patternMatchList(lst, fields)                
          }
        }
      }
    }
  }
    
  def extendEnvWithPat(pat: Pattern, res: RuntimeValue, env: Env): Env = {
    val matched = patternMatch(pat, res)
    var newEnv = env
    for((name,value) <- matched){
      newEnv = newEnv + ((name,value))
    }
    newEnv
    /*
    pat match{
      case SimpleIdent(name) => env + ((name, res))
      case _ => throw new Exception("not impl")
    }
    * */
    
  } 
  
  def evalWithNewEnv(expr: Expr, env: Env): (RuntimeValue, Env) = {
     expr match {
       case Bind(pat, isRef, rand) => {
         val res = evalExpr(rand, env)
         val resWithRef = if (isRef) RTRef(res) else res  
         val newEnv = extendEnvWithPat(pat, resWithRef, env)
         (RTUnit(), newEnv)
       }
       case _ => (evalExpr(expr, env), env)
     }   
  }
  
  def getValueFromEnv(ident: Ident, env: Env): RuntimeValue = {
	 val name = ident.name
	 env.get(name) match {
		 case None => throw new Exception("Variable " + name + " at " + ident.getLoc + " does not exist")
		 case Some(value) => {
			 value //if (rhs){value.asInstanceOf[RTRef].value} else value
		 }
	 }
  }
  
  def evalTuple(tuple: TupleN, env: Env) = {
		  RTTuple(tuple.operands.map(rand => evalExpr(rand, env)))
  }
  
  def evalIfThenElse(ifThenElse: IfThenElseExpr, env: Env): RuntimeValue = {
    val condValue = evalExpr(ifThenElse.cond, env) 
    if (asBoolValue(condValue)){
    	evalExpr(ifThenElse.ifStmt, env)
    }
    else{
        evalExpr(ifThenElse.elseStmt, env)
    }
  }
  
  def evalIfExpr(ifExpr: IfThenExpr, env: Env): RuntimeValue = {
    val condValue = evalExpr(ifExpr.cond, env) 
    if (asBoolValue(condValue)){
    	evalExpr(ifExpr.ifStmt, env)
    }
    else{
        RTUnit()
    }
  }
  
  def evalList(lst: ListInit, env: Env): RuntimeValue = {
    val res = lst.values.map(item => evalExpr(item, env))
    val arrList = new java.util.ArrayList[RuntimeValue]()
    for(item <- res){
      arrList.add(item)
    }
    RTList(arrList)
  }
  
  def evalArrayAccess(access: ArrayAccess, env: Env): RuntimeValue = {
	val lhs = asRTList(evalExpr(access.expr, env))
	val rhs = asIntValue(evalExpr(access.operands(0), env))
	lhs.values.get(rhs)
  }  
  
  def evalAssgnmentStmt(assgnment: AssgnmentStmt, env: Env): RuntimeValue = {
    val lhs = evalExprNoDeref(assgnment.lhs, env)
    val rhs = evalExpr(assgnment.rhs, env)
    asRTRef(lhs).value = rhs
    RTUnit()
  }
  
  def evalPropAccess(access: PropAccess, env: Env): RuntimeValue = {
	val lhs = evalExpr(access.expr, env)
	val rhs = access.name
	lhs match {
	  case RTObject(clazz, instanceVars) => {
	    val idx = clazz.names.indexOf(rhs)
	    if (idx == -1)
	      throw new Exception("field " + rhs + " is not found")
	    instanceVars(idx)
	  }
	  case RTList(lst) => {
	    if (rhs == "length"){
	      RTInt(lst.size())
	    }
	    else{
	      throw new Exception("prop not understood")
	    }
	  }
	  case RTImportedObject(clazz, obj) => {
		  //val clazzObj = Class.forName("java.lang.Object")
		  //println(Class.forName("java.util.ArrayList").getName())
		  //val clazz = l.getClass() // ClassLoader.getSystemClassLoader().loadClass("ArrayList");

		  //val method1: java.lang.reflect.Method = clazz.getMethod(rhs, clazzObj);
		  RTImportedFun(obj, rhs)
		  //method1.invoke(x$1, x$2)
	  }
	}

  }  
  
  def evalWhile(whileExpr: WhileExpr, env: Env): RuntimeValue = {
	while(asBoolValue(evalExpr(whileExpr.cond, env))){
	  evalExpr(whileExpr.body, env)
	}
	RTUnit()
  }
  
  
  def evalExpr(expr: Expr, env: Env): RuntimeValue = {
	val res = evalExprNoDeref(expr, env)
	res match{
	  case RTRef(value) => value
	  case other => other
	}
  }
  //to do: fix () to generate Unit not Tuple()

  def evalExprNoDeref(expr: Expr, env: Env): RuntimeValue = {
	  expr match{
	    case Number(value) => RTInt(value)
	    
	    case Bool(value) => RTBool(value)	    
	    
	    case StringExpr(value) => RTString(value)

	    case list@ListInit(value) => evalList(list, env)
	    
	    case arrayAccess@ArrayAccess(_,_) => evalArrayAccess(arrayAccess, env)
	    
	    case propAccess@PropAccess(_,_) => evalPropAccess(propAccess, env)
	    
	    case ident@Ident(_) => getValueFromEnv(ident, env)
	    
	    case ifThenElse@IfThenElseExpr(_,_,_) => evalIfThenElse(ifThenElse, env)
	    
	    case ifThen@IfThenExpr(_,_) => evalIfExpr(ifThen, env)
	    
	    case tuple@TupleN(_) => evalTuple(tuple, env)
	    
	    case binOp@BinOp(_,_,_) => evalBinOp(binOp, env)
	    
	    case unaryOp@UnaryOp(_,_) => evalUnaryOp(unaryOp, env)
	    
	    case exprs@ExprSeq(lstExpr) => evalBlockOfExpr(lstExpr, env)
	    
	    case funApp@FunApp(_,_) => evalApp(funApp, env)
	    
	    case anonFun@AnonFun(_, _) => evalAnonFun(anonFun)
	    
	    case assgnment@AssgnmentStmt(_, _) => evalAssgnmentStmt(assgnment, env)
	    
	    case whileExpr@WhileExpr(_,_) => evalWhile(whileExpr, env)
	  }
    /*
  case class PatternIdent(val pattern: Pattern) extends Expr
  
  case class BinOp(val name: String, val operand1: Expr, val operand2: Expr) extends Expr
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
  case class ListInit(val lst: List[Expr]) extends Expr
  case class AssgnmentStmt(val lhs: Expr, val rhs: Expr) extends Expr*/
    
  }
  
  def  evalAnonFun(anonFun: AnonFun): RuntimeValue = {
		  RTFun(anonFun.paramNames, anonFun.body)
  }
  
  def evalFunApp(fun: RTFun, funApp: FunApp, env: Env) = {
	  if (funApp.operands.size != fun.argNames.size){
	    throw new Exception("mismamtch with num. args")
	  }
	  val rands = funApp.operands.map(expr => evalExpr(expr, env))
	  
	  val newEnv = extendEnvironment(env, fun.argNames, rands)
	  evalExpr(fun.body, newEnv)    
  }
  
  def evalConstructorApp(clazz: RTClassDef, funApp: FunApp, env: Env): RuntimeValue = {
	  if (funApp.operands.size != clazz.names.size){
	    throw new Exception("mismamtch with num. args")
	  }
	  val rands = funApp.operands.map(expr => evalExpr(expr, env)) 
	  RTObject(clazz, rands)
  }
  
  def evalImportedConstructorApp(classDef: RTImportedClassDef, funApp: FunApp, env: Env): RuntimeValue = {
    /*
	  if (funApp.operands.size != clazz.names.size){
	    throw new Exception("mismamtch with num. args")
	  }*/
	  	
    /*
		  val o = new Object()
     println(o.getClass().getName())
     println(Class.forName("java.lang.Object").getName())
     println(Class.forName("java.util.ArrayList").getName())
	 val clazz = l.getClass() // ClassLoader.getSystemClassLoader().loadClass("ArrayList");
	 val foo = clazz.newInstance()
	 val method1: java.lang.reflect.Method = clazz.getMethod("add", o.getClass());
     fullyQualifiedName
     */
      val clazz = Class.forName(classDef.fullyQualifiedName)
	  val rands = funApp.operands.map(expr => evalExpr(expr, env).toObject) 
	  val objClazz = Class.forName("java.lang.Object")
	 // val argsClazz = funApp.operands.map(_ => objClazz).toArray
	  
	  rands.size match {
        case 0 => { RTImportedObject(clazz, clazz.newInstance().asInstanceOf[java.lang.Object])}
        case 1 => { RTImportedObject(clazz, clazz.getConstructor(objClazz).newInstance(rands(0)).asInstanceOf[java.lang.Object])}
      }
  }
  
  def evalApp(funApp: FunApp, env: Env): RuntimeValue = {
	  val classOrFun = (evalExpr(funApp.expr, env))
	  classOrFun match {
	    case clazz@RTClassDef(_,_) => evalConstructorApp(clazz, funApp, env)
	    case clazz@RTImportedClassDef(_) => evalImportedConstructorApp(clazz, funApp, env)
	    case RTImportedFun(obj, name) => {
		  //val clazzObj = Class.forName("java.lang.Object")
		  //println(Class.forName("java.util.ArrayList").getName())
		  //val clazz = l.getClass() // ClassLoader.getSystemClassLoader().loadClass("ArrayList");
	      val objClass: Class[_] = (new Object()).getClass()
	      val method = obj.getClass().getMethod(name, objClass)
	      val rands = funApp.operands.map(expr => evalExpr(expr, env)) 
	      val res = method.invoke(obj, rands(0).toObject)
	      RTImportedObject(res.getClass(), res)
		  //val method1: java.lang.reflect.Method = clazz.getMethod(rhs, clazzObj);
		 // RTImportedFun(obj, rhs)	      
	    }
	    case fun@RTFun(_,_) => {
	      evalFunApp(fun, funApp, env)
	    }
	  }
  }
  
  def evalFun(funDef: FunDef, args: List[RuntimeValue], env: Env): RuntimeValue = {
	  verifyUniqueNames(funDef.getName, funDef.args)
	  evalExpr(funDef.body, env)
  }
  
  def defToRuntimeValue(adef: Def): RuntimeValue = {
	  adef match {
	    case FunDef(_, args, body) => RTFun(args, body)
	    case ClassDef(name, args) => RTClassDef(name.name, args)
	  }
  }
  
  def extendEnvWithDef(map: Map[String, Def], env: Env) = {
		  val names = map.values.map(adef=> adef.getName).toList
		  val values  = map.values.map(adef => defToRuntimeValue(adef)).toList
		  extendEnvironment(env, names, values)
  }
  
  def evalProgram(program: Program, mainModuleName: String): RuntimeValue = {
	  val emptyEnv = immutable.Map[String, RuntimeValue]()
	  val name2Def = buildMap(program.defs)
	  val mainModule = name2Def.get(mainModuleName).get.asInstanceOf[ModuleDef]
	  val mainModuleMap = buildMap(mainModule.defs)
	  val env = extendEnvWithDef(mainModuleMap, emptyEnv)
	  
	  val env1 = extendEnvironment(env, List("ArrayList"), List(RTImportedClassDef("java.util.ArrayList")))
	  val env2 = extendEnvironment(env1, List("Set"), List(RTImportedClassDef("java.util.HashSet")))
	  val mainFun = mainModuleMap.get("main").get.asInstanceOf[FunDef]
	  evalFun(mainFun, List[RuntimeValue](), env2) //to do: put the names of main funcs in
	  
  }
}