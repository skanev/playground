package layout

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

class SpiralSpec extends Spec with ShouldMatchers {
  def spiral(edges: Int) = Spiral.spiral(edges).toString

  it("renders spirals with 6 edges") {
    spiral(6) should equal (""":+-----
                               :|     
                               :| +-+ 
                               :| + | 
                               :|   | 
                               :+---+ """.stripMargin(':'))
  }

  it("renders a spiral with 11 edges") {
    spiral(11) should equal (""":+----------
                                :|          
                                :| +------+ 
                                :| |      | 
                                :| | +--+ | 
                                :| | |  | | 
                                :| | ++ | | 
                                :| |    | | 
                                :| +----+ | 
                                :|        | 
                                :+--------+ """.stripMargin(':'))
  }

  it("renders a spiral with 17 edges") {
    spiral(17) should equal (""":+----------------
                                :|                
                                :| +------------+ 
                                :| |            | 
                                :| | +--------+ | 
                                :| | |        | | 
                                :| | | +----+ | | 
                                :| | | |    | | | 
                                :| | | | ++ | | | 
                                :| | | |  | | | | 
                                :| | | +--+ | | | 
                                :| | |      | | | 
                                :| | +------+ | | 
                                :| |          | | 
                                :| +----------+ | 
                                :|              | 
                                :+--------------+ """.stripMargin(':'))
  }
}
