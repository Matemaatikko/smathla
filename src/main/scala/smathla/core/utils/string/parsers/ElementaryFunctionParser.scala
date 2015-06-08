package smathla.core.utils.string.parsers

import smathla.core.Types
import Types.ElementaryModifier
import smathla.calculus.elementary._
import smathla.core.utils.{Token, Lexer}

import scala.collection.mutable.Buffer


//TODO remake parser
/**
 * This object uses parse-method to parse Function from given string.
 * Note: When writing functions, brachet's are needed to mark inner function.
 */
object ElementaryModifierParser{

  /**
   * This method first create Lexer to split given string to Token-array.
   * And then calls another parse-method to generate tree from array.
   * Parsed tree is returned.
   */
  def parse(input: String): ElementaryModifier = {
    val lexer = new Lexer(input.toList)
    lexer.run
    val list: Buffer[Token] = lexer.getTokens
    check(list)
    return ??? //parse(list.asInstanceOf[Buffer[Any]])
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
  def add(a: String, b: ((ElementaryModifier) => ElementaryModifier)) {
    maps = maps + ((a, b))
  }

  /**
   * Maps function names to functions.
   */
  private var maps = Map[String, (ElementaryModifier) => ElementaryModifier](
    "sin" -> (in => new Sin(in)),
    "cos" -> (in => new Cos(in)),
    "tan" -> (in => new Tan(in)),
    "exp" -> (in => new Exp(in)),
    "log" -> (in => new Log(in))
  )

  /**
   * Returns function which inner function is [in] and it's name match with [token].
   */
  private def map(token: String, in: ElementaryModifier): ElementaryModifier = {
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
 /* private def parse(buffer: Buffer[Any]): ElementaryModifier = {
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
          buffer(pos) = new Constant(token.asInstanceOf[smathla.core.utils.Integer].l)
        }
        case Token(Token.Real) => {
          buffer(pos) = new Constant(token.asInstanceOf[smathla.core.utils.Real].l)
        }
        case Token(Token.Letter) => {
          buffer(pos) = new Variable(token.value.charAt(0))
        }
        case Token(Token.Word) => {
          var cat: ElementaryModifier = null
          //Note here that parser fails in condition function1 unary_minus something. That means that brachet's are needed to mark inner function.
          val in: ElementaryModifier = buffer(pos + 1).asInstanceOf[ElementaryModifier]
          cat = map(token.value, in)
          buffer(pos) = cat
          buffer.remove(pos + 1, 1) //Removes inner function..
        }
        case Token('-') if token.isUnary => {
          buffer(pos) = new Multiplication(new Constant(-1), buffer(pos + 1).asInstanceOf[ElementaryModifier])
          buffer.remove(pos + 1, 1)
        }
        case t: Token if List('^', '*', '/', '+', '-').contains(token.value) => {
          val info_fst = buffer(pos - 1).asInstanceOf[ElementaryModifier]
          val info_snd = buffer(pos + 1).asInstanceOf[ElementaryModifier]
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
    return buffer(0).asInstanceOf[ElementaryModifier]
  }*/
}