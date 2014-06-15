package week6

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.scalatest.WordSpec

class CollectionsTest extends WordSpec with ShouldMatchers {

            
	"Demo of Scala Collections" should {
        "Experiment with Ranges" in {
            (5 to 0 by -1) should equal (List(5, 4, 3, 2, 1, 0))
            (5 until 0 by -1) should equal (List(5, 4, 3, 2, 1))
        }
        
        "Experiment with Zip" in {
            (1 to 3) zip (3 until 0 by -1) should equal (Vector((1, 3), (2, 2), (3, 1)))
        }
        
        "Experiment with Vector.forall" in {
            Vector((1, 3), (2, 2), (3, 1)) forall {
                case (x, y) => x + y == 4
            }
        }
        
        "Experiment with for-expression" in {
            val v = Vector((1, 3), (2, 2), (3, 1))
                        
            val l = for {
                (x, y) <- v
                if (x >= y)
            } yield (x, y)
            
            l should (contain((2, 2)) and (contain(3, 1)))
        }
        
        "Experiment with Take" in {
            val l = List (1, 2, 3, 4)
            l take 2 should equal (List(1, 2))
        }
        
        "Experiment with Drop" in {
            val l = List (1, 2, 3, 4)
            l drop 2 should equal (List(3, 4))
        }
        
        "Experiment with GroupBy" in {       
            def reverse (s: String) = s.reverse
            
            val words = List("Hello", "World")
            words groupBy reverse should be (Map("olleH" -> List("Hello"), "dlroW" -> List("World")))
        }
        
        "Experiment with Filter" in {
            val l = List ("Howdy", "Y'all")
            val words = l filter (word => word forall (ltr => ltr.isLetter))
            
            words should have length (1)
            words should contain ("Howdy")
            words should not contain ("Y'all")
        }
	}
}