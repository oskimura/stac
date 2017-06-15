import scala.collection.mutable.Stack
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._


//----------------------------------------------------------------------------------------------
abstract class Instraction
case class POP[A]        extends Instraction
case class PUSH          extends Instraction
case class EXCH          extends Instraction
case class ADD[A <: Int] extends Instraction
case class VAR           extends Instraction

abstract class StackVal
case class  StrVal[A <: String](v:A)      extends StackVal
case class  CharVal[A <: Char](v:A)       extends StackVal
case class  IntVal[A <: Int](v:A)         extends StackVal
case class  BoolVal[A <: Boolean](v:A)    extends StackVal
case class  RealVal[A <: Double](v:A)     extends StackVal
case class  Name(v:String)                extends StackVal
case class  Dict(m : Map[Name, StackVal])     extends StackVal
case class  ArrayVal(v:List[StackVal])       extends StackVal
case class  ExecArray[+Instr](v:List[Instr])   extends StackVal
case object Mark                       extends StackVal
case object Null                       extends StackVal

//--------------------------------------------------------------------
class DictionaryStack {
  val m = new Stack(): Stack[Dict]
  m.push(Dict(Map()))
  def push(d:Dict) {
    m.push(d)
  }
  val pop = ()=>{ m.pop }

  def lookup(n:Name): Option[StackVal] = {
    val e = m.find( e => e.m.contains(n) )
    e match {
      case None    => None
      case Some(x) => x.m.get(n)
    }
  }
 override def toString():String = {
    return m.toString()
  }
}

//-----------------------------------------------------------------------------
abstract  class Instr
case object Pop               extends Instr
case class Push(v:StackVal)   extends Instr
case object Exch              extends Instr
case object Dup               extends Instr
case object If                extends Instr
case object IfElse            extends Instr
case object Exec              extends Instr
case object Def               extends Instr
case object Load              extends Instr
case object Stor              extends Instr
case object Begin             extends Instr
case object End               extends Instr
case object Loop              extends Instr
case object Exit              extends Instr
case class  Cvx(p:ExecArray[Instr])  extends Instr

case object Dynamic         extends Instr

case object Add             extends Instr
case object Sub             extends Instr
case object Mul             extends Instr

case object Eq              extends Instr
//---------------------------------------------------------------------------
class Enviroment {
  val operand    = new Stack():  Stack[StackVal]
  val execute    = new Stack():  Stack[Instr]
  val dictionary = new DictionaryStack():  DictionaryStack

  def pushExec(a: ExecArray[Instr]) = {

    a match {
      case ExecArray(is:List[Instr]) => for (i <- is.reverse) {execute.push(i)}
      case _                         => println("abort")
    }
  }


  def print() = {
    println("op   " ++ operand.toString())
    println("exe  " ++ execute.toString())
    println("dict " ++ dictionary.toString())
  }

}

//-----------------------------------------------------------------------------------
object StackParser extends StandardTokenParsers  {
  lexical.reserved += ( "true","false","if", "ifelse", "exit","loop","pop","push","p", "mark" , "add", "sub", "mul", "eq", "exec"
                      , "exch","dup", "def", "begin", "end", "dynamic")
  lexical.delimiters +=(" ","/", "{","}","[","]", "<<", ">>" )

  lazy val pop    : Parser [Instr] =
    ("pop") ^^^ Pop
  lazy val bool   : Parser [StackVal] =
    ("true"|"false") ^^ { case "true" => BoolVal(true)
			  case "false" => BoolVal(false)
			}

  lazy val int    : Parser[StackVal] =
    numericLit ^^ {case s => IntVal(s.toInt)}

  lazy val str    : Parser[StackVal] =
    stringLit ^^ {case s => StrVal(s.toString)}

  lazy val value  : Parser[StackVal] =
    (bool | int | str | mark | name | compval)

  lazy val push   : Parser[Instr] =
    value ^^ { case v =>  Push(v)}

  lazy val compval : Parser[StackVal] =
    (dict | array | execArray )

  lazy val array  : Parser[StackVal] =
    ("[" ~> rep(value) <~ "]") ^^ {case s => ArrayVal(s)}

  lazy val execArray : Parser[ExecArray[Instr]] =
    "{" ~> terms <~ "}" ^^ {case s => ExecArray (s)}

  lazy val if_       : Parser[Instr] =
    ("if")  ^^^ If

   lazy val ifelse : Parser[Instr]  =
    ("ifelse") ^^^ {IfElse}

  lazy val add  : Parser[Instr]  =
    "add" ^^^ {Add}

  lazy val sub  : Parser[Instr]  =
    "sub" ^^^ {Sub}

  lazy val mul  : Parser[Instr]  =
    "mul" ^^^ {Mul}

  lazy val eq  : Parser[Instr]  =
    "eq" ^^^ {Eq}

  lazy val loop   : Parser[Instr]  =
    "loop" ^^^ {Loop}

  lazy val exit   : Parser[Instr]  =
    ("exit") ^^^ {Exit}

  lazy val mark   : Parser[StackVal]  =
    ("mark") ^^^ {Mark}

  lazy val name   : Parser[Name]  =
    ("/" ~> ident) ^^ { case s => Name(s) }

  lazy val pair   : Parser[(Name, StackVal)] =
    (name ~ value) ^^ {case n ~ v => (n,v) }

  lazy val dict   : Parser[StackVal]  =
    ("<<" ~> rep1(pair) <~ ">>") ^^ { case (xs) => Dict(xs.toMap) }

  lazy val def_ : Parser[Instr] =
    ("def") ^^^ {Def}

  lazy val begin  : Parser[Instr] =
    "begin" ^^^ {Begin}

  lazy val end    : Parser[Instr] =
    "end" ^^^ {End}

  lazy val exec : Parser[Instr] =
    "exec" ^^^ {Exec}

  lazy val exch : Parser[Instr] =
    "exch" ^^^ {Exch}

  lazy val dup : Parser[Instr] =
    "dup" ^^^ {Dup}


  lazy val dynamic :Parser[Instr] =
    ("dynamic") ^^^ {Dynamic}

  lazy val sym : Parser[StackVal] =
    ident ^^ { case s => Name(s) }

  lazy val term   : Parser[Instr]  =
    (exit | begin | end | push  | pop | add | sub | mul | eq | loop | if_ | ifelse | exec | exch | dup | def_ | begin | end | dynamic)

  lazy val compterm : Parser[Instr] =
    (if_)

  lazy val terms  : Parser[List[Instr]] =
    rep1(term)
   def parse(text : String)(p : List[Instr]=>Unit) = {
       val scanner = new StackParser.lexical.Scanner(text)
       val result  = StackParser.terms(scanner);

       result match {
	 case Success(result, x) => println("sucess : " ++ result.toString ++ " / "++ x.toString);
				    p(result)
	 case Failure(msg, _)    => println("failure : " ++ msg)
	 case Error(msg, _)      => println("error : " ++ msg)
       }
   }
}


//-------------------------------------------------------------------------
object Evaluator {
def exit_(e: Enviroment):Enviroment =  {
  val p = e.execute.pop
  println(p)
   p match {
     case Loop => return e;
     case _    => return exit_(e)
  }
}

  def eval(i:Instr, e:Enviroment): Enviroment = {
    println(i)

    i match {
      case Pop     =>
	e.operand.pop;

      case Push(v) =>
	println("push");e.operand.push(v) ;

      case Exch =>
	val v1 = e.operand.pop; val v2 = e.operand.pop;
      e.operand.push(v1); e.operand.push(v2);

      case Dup =>
	val v1 = e.operand.pop; e.operand.push(v1);e.operand.push(v1);

      case If =>
	val p = e.operand.pop; val c = e.operand.pop
      (c,p) match {
	case (BoolVal(c),p:ExecArray[Instr])  => if (c) {e.pushExec(p)}
      }

      case IfElse  =>
	val p1 = e.operand.pop; val p2 = e.operand.pop; val c = e.operand.pop
      (c,p1,p2) match {
	case (BoolVal(c),p1:ExecArray[Instr],p2:ExecArray[Instr])  => if (c) {e.pushExec(p1)} else  {e.pushExec(p2)}
      }

      case Def =>
	val v = e.operand.pop; val s = e.operand.pop;
	(s,v) match {
	  case (n:Name,vs:StackVal) =>

	    e.dictionary.m.pop.m match {
	      case d:Map[Name,StackVal] =>

		e.dictionary.push(Dict(d + (n -> vs)));
	    }
	}
      // for dictionar stack
      case Begin  =>
	val v = e.operand.pop
      v match {
	case Dict(d)  =>  e.dictionary.push(Dict(d)) ;
	case _        => println("unmatch type.")
      }

      case End    =>
	e.dictionary.pop ;

      //  for execute stack
      case Exec  =>
	val p = e.operand.pop
      p match { case ps:ExecArray[Instr] => e.pushExec(ps) }


      // loop proc
      case Loop  =>
	val p = e.operand.pop
      p match { case ExecArray(ps:List[Instr]) => e.pushExec(ExecArray[Instr](Loop::ps)) }


      case Exit   =>
	exit_(e);

      case Dynamic  =>
	val n = e.operand.pop
      n match {
	case n:Name =>
	  e.dictionary.lookup(n) match {
	    case Some(v) => println("found " ++ v.toString);e.operand.push(v)
	    case None    => println("unboud symbol" ++ n.toString)
	  }
      }


      case Add =>
	val v1 = e.operand.pop; val v2 = e.operand.pop;
      (v1,v2) match { case (IntVal(n1),IntVal(n2)) => e.operand.push(IntVal[Int](n1+n2)); }

      case Sub =>
	val v1 = e.operand.pop; val v2 = e.operand.pop;
      (v1,v2) match { case (IntVal(n1),IntVal(n2)) => e.operand.push(IntVal[Int](n2-n1)); }

      case Mul =>
	val v1 = e.operand.pop; val v2 = e.operand.pop;
      (v1,v2) match { case (IntVal(n1),IntVal(n2)) => e.operand.push(IntVal[Int](n1*n2)); }

      case Eq =>
	val v1 = e.operand.pop; val v2 = e.operand.pop;
      (v1,v2) match { case (IntVal(n1),IntVal(n2)) => e.operand.push(BoolVal(n1==n2)); }


      case _       =>
	println("this instruction not implements " ++ i.toString ++ " .");
    }

    e.print
    return e;
  }



  def run(e:Enviroment):Enviroment = {
    println("run")
    if (!e.execute.isEmpty) {
      val i = e.execute.pop;
      val e2 = eval(i,e)
      return run(e2);
    }
    return e
  }

  def parse(str:String)  = {
    val e = new Enviroment
    StackParser.parse(str)((x:List[Instr]) => { e.pushExec(ExecArray(x)); run(e);});
  }

  def cvx(ps:List[StackVal]):ExecArray[Instr] = {
    return ExecArray(ps.map((x)=> Push(x)))
  }

  def repl() = {
    var e = new Enviroment
    print("> ")
    var line = readLine

    while (line != "quit") {

      StackParser.parse(line)((x:List[Instr]) =>
	{ 
    e.pushExec(ExecArray(x));
	  e =  run(e);
        });

      print("> ")
      line = readLine
    }
  }
}

class stac {
  def parse(str:String)  = {
    Evaluator.parse(str);
  }

  def repl() = {
    Evaluator.repl
  }
}

// /fact { dup 1 eq  { dup 1 sub  /fact dynamic  exec mul } { pop 1 } ifelse }  def 10 /fact dynamic exec
