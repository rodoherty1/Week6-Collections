package week6

/*
 * My semi-blind attempt at recording the N-Queens problem
 */
object NQueens2 {
    
    def apply(n: Int): Set[List[Int]] = {
	    def isSafe(col: Int, queens: List[Int]): Boolean = {
	        val row = queens.length
	        val queensWithRow = (queens.length-1 to 0 by -1) zip queens
	        
	        queensWithRow forall {
	            case(r, c) => r != row && c != col && (math.abs(r - row) != math.abs(c - col))
	        }
	        
	    }
	    
	    def placeQueens(k: Int): Set[List[Int]] = {
		    if (k == 0) Set(List())
		    else {
			    for {
		            rows <- placeQueens(k-1)
		            col <- 0 until n
		            if (isSafe(col, rows))
		        } yield col :: rows
		    }
	    }
	    placeQueens(n)
    }
}