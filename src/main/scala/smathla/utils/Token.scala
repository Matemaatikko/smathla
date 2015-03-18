package smathla.utils

//TODO clean
/**
 * This class is tool class for lexer class.
 * There are six different type tokens:
 * 1. Int
 * 2. Real
 * 3. Letter
 * 4. Word
 * 5. operators
 * 6. unary operators.
 */

class Token(s: String, ind: Int, unary: Boolean = false){
   /**
    * Variable: ind is Int value of given operator (Char). And otherwise its
    * 0: Int
    * 1: Real
    * 2: Letter
    * 3: Word
    */
  
    /**
     * When this token is operator, then variable unary tells is it unary operator.
     */ 
  
    /**
     * Returns string value of the Token.
     */
    def value = s
    
    /**
     * Returns index of the token.
     */
    def index = ind
     

    def isUnary = unary
    override def toString = if(!unary) s else s+".true"
}
case class Integer(s: String, ind: Int) extends Token(s: String, ind: Int){
  val l:Long = s.toLong
  def this(l :Long){
    this(l.toString(), 0) 
    val this.l = l
   }
}
case class Real(s: String, ind: Int) extends Token(s: String, ind: Int){
  val l:Double = s.toDouble
  def this(l :Double){
    this(l.toString(), 1) 
    val this.l = l
   }
}
object Token{
	val Int = 0
	val Real = 1
	val Letter = 2
	val Word = 3
	def unapply(that: Token) = Some(that.index)
}