package week6

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class ScalaIsFunTest extends WordSpec with ShouldMatchers {
    
    "Translate mnemonics into Phrases" should {
        """translate 7225247386 into "scala is fun"""" in {
            ScalaIsFun("7225247386") should contain ("scala is fun")
        }
    }

}