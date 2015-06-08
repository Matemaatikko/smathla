package smathla.core.utils

import scala.collection.mutable.Buffer

//TODO clean
/**
 * This class split given char-array to Token-array where are following parts:
 * 1. numbers
 * 2. words, letters
 * 3. operators
 * 4. unary operators.
 * Example: For argument "3.5+5sin(3x)+-3".toList result is like {3.5,+,5,sin,(,3,x,),+,-.unary,3}.
 */
class Lexer(input: List[Char]) {
  
	var tokens : Buffer[Token] = Buffer()
	var pos = 0
	var unary = true
	
	/**
	 * Return result after run-method is executed.
	 */
	def getTokens = tokens
	
	def get() {
	  var c = input(pos)
	  var previous = pos
	  do{
	    previous = pos
	    jumpper(c)
	    c = input(pos)    
	  } while(previous != pos)
	  c match {
	    case it if it.isDigit => getNumber(c)
	    case it if it.isLetter => getWord(c)
	    case _ => {
	      tokens += new Token(c.toString, c, unary)
	      unary = true
	      pos += 1
	    }
	  }
	}
	
	/**
	 * Runs lexer.
	 */
	def run() {
	  while(pos < input.size) get
	}
	
	/**
	 * This method jumps over whitespace characters.
	 */
	def jumpper(c : Char){
	  c match {
	    case ' ' => pos +=1
	    case '\t' => pos +=1
	    case '\n' => pos +=1
	    case _ =>
	  }
	}

	def getWord(c: Char){
	  var a = new StringBuilder()
	  a += c
	  pos += 1
	  if(pos >= input.size){
	    tokens += new Token(a.toString, Token.Letter)
	    return
	  }
	  var ca = input(pos)
	  while(ca.isLetter){
	    a += ca
	    pos += 1
	    if(pos >= input.size){
	      ca = '1'
	    }
	    else ca = input(pos)
	  }
	  if(a.length == 1) tokens += new Token(a.toString, Token.Letter)
	  else tokens += new Token(a.toString, Token.Word)
	  unary = false
	}
	
	def getNumber(c: Char){
	  var a : Long = c.asDigit
	  pos += 1
	  if(pos >= input.size) {
	    tokens += new Integer(a)
	    return
	  }
	  var ca = input(pos)
	  def in_loop() {
	    a = a*10 + ca.asDigit
	    pos += 1
	    if(pos >= input.size) ca = 'a'
	    else ca = input(pos)    
	  }
	  while(ca.isDigit){
	    in_loop
	  }
	  if(ca == '.'){
	      var des = 1.0
	      pos += 1
	      ca = input(pos)
		  while(ca.isDigit){
			  in_loop
			  des *= 10
		  }
		  if(des == 1){
		    tokens += new Integer(a)
		  } else {
		    tokens += new Real(a/des)
		  }
	  } else {
	    tokens += new Integer(a)
	  }
	  unary = false
	}
}