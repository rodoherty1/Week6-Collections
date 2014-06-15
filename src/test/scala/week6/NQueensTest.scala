package week6

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class NQueensTest extends WordSpec with ShouldMatchers {

    "NQueens" should {
        "Place 4 queens on a chess board without threatening each other" in {
            val solutions = NQueens2(4)
            solutions should (contain (List(2, 0, 3, 1)) and (contain (List(1, 3, 0, 2))))
        }

    }
    
}