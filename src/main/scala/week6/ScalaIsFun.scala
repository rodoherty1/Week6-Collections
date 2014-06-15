package week6

import scala.io.Source

object ScalaIsFun {

    def apply(number: String): Set[String] = {
	    val in = Source.fromFile("/home/rob/words.txt", "UTF-8")
	    val words = in.getLines.toList filter (word => word forall (ltr => ltr.isLetter && ltr < 'z'))
	    
	    val nmem = Map(
	            '2' -> "ABC",
	            '3' -> "DEF",
	            '4' -> "GHI",
	            '5' -> "JKL",
	            '6' -> "MNO",
	            '7' -> "PQRS",
	            '8' -> "TUV",
	            '9' -> "WXYZ")
	            
	    val charCode : Map[Char, Char] = for {
	        (digit, ltrs) <- nmem
	        ltr <- ltrs
	    } yield ltr -> digit
	
	    def wordCode (word: String) : String = {
	    	word.toUpperCase map charCode
	    }
	    
	    val wordsForNum : Map[String, Seq[String]] = 
	        words groupBy wordCode withDefaultValue Seq()
	        
	    def encode (number : String) : Set[List[String]] = {
	        if (number.length == 0) Set(List())
	        else {
	            {
			        for {
			        	splitPoint <- (1 to number.length)
			            word <- wordsForNum (number take splitPoint)
			            rest <- encode (number drop splitPoint)
			        } yield word :: rest
	            }.toSet
	        }
	    }
	    
	    def translate(solutions: Set[List[String]]): Set[String] = {
	        solutions map (_ mkString " ")
	    }
	    
	    translate(encode(number))
    }       
}