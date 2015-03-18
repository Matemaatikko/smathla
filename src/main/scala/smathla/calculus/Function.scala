package Math.Calculus

import smathla.utils.{Token, Lexer}

import scala.collection.mutable.Buffer


//TODO remake
/**
 *  Replace scala values with Integer, Real and so on.
 */

/**
 * This class identifies differentiable mathematical function: R -> R.
 */
abstract class Function { //TODO rename to 'alkeisfunktio'

  def apply(a: Double) = value(a)

  /**
   * Returns value of function with given argument.
   */
  def value(a: Double): Double

  /**
   * Returns new function where Variable defined by char c is replaced with Constant defined by number a.
   */
  def substitution(a: Double, c: Char): Function

  def +(f: Function) = new Addition(this, f)

  def -(f: Function) = new Subtraction(this, f)

  def *(f: Function) = new Multiplication(this, f)

  def /(f: Function) = new Division(this, f)

  def ^(f: Function) = new Power(this, f)

  def ^(d: Double) = new Power(this, new Constant(d))

  /**
   * Returns derivative of function.
   */
  def derive: Function

  /**
   * Method uses reduceOne method to reduce function as long as it could be reduced.
   */
  def reduce: Function = {
    var f = this
    var last = this
    do {
      last = f
      f = f.reduceOnce
    } while (f.toString != last.toString)
    return f
  }

  /**
   * Removes useless parts from function. 
   * Example: 0.0 + 1.0*x -> x
   */
  protected def reduceOnce: Function
}

/**
 * Next we implement some basic relations:
 * Multiplication
 * Division
 * Addition
 * Subtraction
 * Power
 *
 * and some common functions:
 * Sin
 * Cos
 * Tan
 * Exp
 * Log
 */

case class Multiplication(fst: Function, snd: Function) extends Function {
  def value(a: Double) = fst(a) * snd(a)

  def substitution(a: Double, c: Char) = new Multiplication(fst.substitution(a, c), snd.substitution(a, c))

  def derive = fst.derive * snd + fst * snd.derive

  override def toString() = "(" + fst.toString + ")*(" + snd.toString + ")"

  def reduceOnce: Function = {
    fst match {
      case Constant(0.0) => return new Constant(0)
      case Constant(1.0) => return snd.reduce
      case _ =>
    }
    snd match {
      case Constant(0.0) => return new Constant(0)
      case Constant(1.0) => return fst.reduce
      case _ =>
    }
    return fst.reduce * snd.reduce
  }
}

case class Division(fst: Function, snd: Function) extends Function {
  def value(a: Double) = fst(a) / snd(a)

  def substitution(a: Double, c: Char) = new Division(fst.substitution(a, c), snd.substitution(a, c))

  def derive = (fst.derive * snd - fst * snd.derive) / (snd ^ 2)

  override def toString() = "(" + fst.toString + ")/(" + snd.toString + ")"

  def reduceOnce: Function = {
    fst match {
      case Constant(0.0) => return new Constant(0)
      case _ =>
    }
    return fst.reduce / snd.reduce
  }
}

case class Addition(fst: Function, snd: Function) extends Function {
  def value(a: Double) = fst(a) + snd(a)

  def substitution(a: Double, c: Char) = new Addition(fst.substitution(a, c), snd.substitution(a, c))

  def derive = fst.derive + snd.derive

  override def toString() = "" + fst.toString + "+" + snd.toString + ""

  def reduceOnce: Function = {
    fst match {
      case Constant(0.0) => return snd.reduce
      case _ =>
    }
    snd match {
      case Constant(0.0) => return fst.reduce
      case _ =>
    }
    return fst.reduce + snd.reduce
  }
}

case class Subtraction(fst: Function, snd: Function) extends Function {
  def value(a: Double) = fst(a) - snd(a)

  def substitution(a: Double, c: Char) = new Subtraction(fst.substitution(a, c), snd.substitution(a, c))

  def derive = fst.derive - snd.derive

  override def toString() = "" + fst.toString + "-" + snd.toString + ""

  def reduceOnce: Function = {
    fst match {
      case Constant(0.0) => return new Constant(-1) * snd.reduce
      case _ =>
    }
    snd match {
      case Constant(0.0) => return fst.reduce
      case _ =>
    }
    return fst.reduce - snd.reduce
  }
}

case class Power(base: Function, to: Function) extends Function {
  def value(a: Double) = base(a) - to(a)

  def substitution(a: Double, c: Char) = new Subtraction(base.substitution(a, c), to.substitution(a, c))

  def derive = (base ^ to) * (to / base + to.derive * Log(base))

  override def toString() = "(" + base.toString + ")^(" + to.toString + ")"

  def reduceOnce = new Power(base.reduce, to.reduce)
}

case class Variable(v: Char) extends Function {
  def value(a: Double) = a

  def substitution(a: Double, c: Char) = if (c == v) new Constant(a) else this

  def derive = Constant(1.0)

  override def toString() = v.toString()

  def reduceOnce = this
}

case class Constant(d: Double) extends Function {
  def value(a: Double) = d

  def substitution(a: Double, c: Char) = this

  def derive = Constant(0.0)

  override def toString() = d.toString()

  def reduceOnce = this
}

case class Sin(f: Function) extends Function {
  def value(a: Double) = math.sin(f(a))

  def substitution(a: Double, c: Char) = new Sin(f.substitution(a, c))

  def derive = Cos(f) * f.derive

  override def toString() = "sin(" + f.toString + ")"

  def reduceOnce = Sin(f.reduce)
}

case class Cos(f: Function) extends Function {
  def value(a: Double) = math.cos(f(a))

  def substitution(a: Double, c: Char) = new Cos(f.substitution(a, c))

  def derive = new Constant(-1) * Sin(f) * f.derive

  override def toString() = "cos(" + f.toString + ")"

  def reduceOnce = Cos(f.reduce)
}

case class Tan(f: Function) extends Function {
  def value(a: Double) = math.tan(f(a))

  def substitution(a: Double, c: Char) = new Tan(f.substitution(a, c))

  def derive = (new Constant(1) + new Tan(f) ^ 2) * f.derive

  override def toString() = "tan(" + f.toString + ")"

  def reduceOnce = Tan(f.reduce)
}

case class Exp(f: Function) extends Function {
  def value(a: Double) = math.exp(f(a))

  def substitution(a: Double, c: Char) = new Exp(f.substitution(a, c))

  def derive = Exp(f) * f.derive

  override def toString() = "exp(" + f.toString + ")"

  def reduceOnce = Exp(f.reduce)
}

case class Log(f: Function) extends Function {
  def value(a: Double) = math.log(f(a))

  def substitution(a: Double, c: Char) = new Log(f.substitution(a, c))

  def derive = f.derive / f

  override def toString() = "log(" + f.toString + ")"

  def reduceOnce = Log(f.reduce)
}

/**
 * This object uses parse-method to parse Function from given string.
 * Note: When writing functions, brachet's are needed to mark inner function.
 */
object Parser {

  def main(args: Array[String]) {
    var list = List("sin(x)", "cos(x)", "tan(x)", "exp(x)", "log(x)", "x^2", "-x", "-3.24-x^x", "5x^x-3x^2+2x-1")
    for (a <- list) {
      var f = parse(a)
      println("String: " + a)
      println("Function: " + f.toString)
      println("Derivate: " + f.derive)
      println("Reduced derivate: " + (f.derive).reduce)
      println("\n")
    }
  }

  /**
   * This method first create Lexer to split given string to Token-array.
   * And then calls another parse-method to generate tree from array.
   * Parsed tree is returned. 
   */
  def parse(input: String): Function = {
    val lexer = new Lexer(input.toList)
    lexer.run
    var list: Buffer[Token] = lexer.getTokens
    check(list)
    return parse(list.asInstanceOf[Buffer[Any]])
  }

  /**
   * If list contains [Token.{Int or Real}, Token.{Letter or Word}] as a sublist, then this method inserts
   * Token("*", '*') between them.  
   */
  private def check(list: Buffer[Token]) {
    var digit = false
    for (a <- 0 until list.size) {
      if (digit && (list(a).index == Token.Letter || list(a).index == Token.Word)) {
        list.insert(a, new Token("*", '*'))
      }
      if (list(a).index == Token.Int || list(a).index == Token.Real) digit = true
      else digit = false
    }
  }

  /**
   * Method returns next opening brachet's position starting at given position.
   * If there is opening brachet in given position then closing pair brachet's position is returned.
   * Otherwise returns -1.
   *
   * Brachet's need to match with Token('(') or Token(')').
   */
  private def next(buffer: Buffer[Any], pos: Int): Int = {
    var count = 0
    if (pos != -1) {
      buffer(pos) match {
        case Token('(') => count += 1
        case _ =>
      }
    }
    for (a <- pos + 1 until buffer.size) {
      buffer(a) match {
        case Token('(') => {
          if (count == 0) return a
          count += 1
        }
        case Token(')') => {
          count -= 1
          if (count == 0) return a
        }
        case _ =>
      }
    }
    return -1
  }

  /**
   * Returns next Token in buffer that match with Token(char). If unary == true, then also needed that <token>.isUnary == true
   */
  private def get(buffer: Buffer[Any], char: Int, unary: Boolean = false): Int = {
    for (a <- 0 until buffer.size) {
      buffer(a) match {
        case Token(it) if it == char && (!unary || buffer(a).asInstanceOf[Token].isUnary) => return a
        case _ =>
      }
    }
    return -1
  }

  /**
   * Method adds new function definition to variable maps. 
   */
  def add(a: String, b: ((Function) => Function)) {
    maps = maps + ((a, b))
  }

  /**
   * Maps function names to functions.
   */
  private var maps = Map[String, (Function) => Function](
    "sin" -> (in => new Sin(in)),
    "cos" -> (in => new Cos(in)),
    "tan" -> (in => new Tan(in)),
    "exp" -> (in => new Exp(in)),
    "log" -> (in => new Log(in))
  )

  /**
   * Returns function which inner function is [in] and it's name match with [token].
   */
  private def map(token: String, in: Function): Function = {
    if (maps.contains(token)) return maps(token).apply(in)
    else return null
  }


  /**
   * Parses given [Token,Function]-array to Function tree.
   * Example: {3.5,+,5,*,sin,(,3,*,x,),+,-,3} ->
   * Sum(Sum(Const(3.5), Sin(Mul(Const(3), Var(x)))), Mul(Const(-1), Const(3)))
   *
   * In parsing we follow following calculation order:
   * 1. brachet's (,)
   * 2. constants
   * 3. variables
   * 4. functions
   * 5. unary minus
   * 6. ^
   * 7. /
   * 8. *
   * 9. +
   * 10.-
   */
  private def parse(buffer: Buffer[Any]): Function = {
    while (next(buffer, 0) != -1) {
      // This part of code removes brachet's from array, parsing inner parts of brachet's.
      val begin = next(buffer, -1)
      val end = next(buffer, begin)
      val ca = parse(buffer.slice(begin + 1, end))
      buffer(begin) = ca
      buffer.remove(begin + 1, end - begin)
    }

    /**
     * This method handles token in given position.
     * That means:
     * 1. Numbers are translated to Constants.
     * 2. Letters are translated to Variables.
     * 3. Words are translated to common functions that they match. (function after word is given as inner function to new function)
     * 4. Operators are translated to Basic-relations. (operator operates with functions that are after and before operator)
     */
    def handle(pos: Int) {
      require(buffer(pos).isInstanceOf[Token])
      def token = buffer(pos).asInstanceOf[Token]
      token match {
        case Token(Token.Int) => {
          buffer(pos) = new Constant(token.asInstanceOf[smathla.utils.Integer].l)
        }
        case Token(Token.Real) => {
          buffer(pos) = new Constant(token.asInstanceOf[smathla.utils.Real].l)
        }
        case Token(Token.Letter) => {
          buffer(pos) = new Variable(token.value.charAt(0))
        }
        case Token(Token.Word) => {
          var cat: Function = null
          //Note here that parser fails in condition function1 unary_minus something. That means that brachet's are needed to mark inner function.
          val in: Function = buffer(pos + 1).asInstanceOf[Function]
          cat = map(token.value, in)
          buffer(pos) = cat
          buffer.remove(pos + 1, 1) //Removes inner function..
        }
        case Token('-') if token.isUnary => {
          buffer(pos) = new Multiplication(new Constant(-1), buffer(pos + 1).asInstanceOf[Function])
          buffer.remove(pos + 1, 1)
        }
        case t: Token if List('^', '*', '/', '+', '-').contains(token.value) => {
          val info_fst = buffer(pos - 1).asInstanceOf[Function]
          val info_snd = buffer(pos + 1).asInstanceOf[Function]
          val c = token.value
          c.charAt(0) match {
            case '^' => buffer(pos) = new Power(info_fst, info_snd)
            case '/' => buffer(pos) = new Division(info_fst, info_snd)
            case '*' => buffer(pos) = new Multiplication(info_fst, info_snd)
            case '+' => buffer(pos) = new Addition(info_fst, info_snd)
            case '-' => buffer(pos) = new Subtraction(info_fst, info_snd)
          }
          buffer.remove(pos + 1, 1)
          buffer.remove(pos - 1, 1)
        }
      }
    }
    for (c <- List(0, 1, 2, 3)) {
      while (get(buffer, c) != -1) {
        handle(get(buffer, c))
      }
    }
    while (get(buffer, '-', true) != -1) handle(get(buffer, '-', true))
    for (c <- List('^', '/', '*', '+', '-')) {
      while (get(buffer, c) != -1) handle(get(buffer, c))
    }
    return buffer(0).asInstanceOf[Function]
  }
}