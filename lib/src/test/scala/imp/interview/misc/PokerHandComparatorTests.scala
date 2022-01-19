package imp.interview.misc

import org.junit.Assert
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PokerHandComparatorTests extends AnyFunSuite {
  test("poker hand comparator") {
    val phc = new PokerHandComparator
    Assert.assertEquals(false, phc.validateCard("AKS"))
    Assert.assertEquals(true, phc.validateCard("AD"))
    Assert.assertEquals(true, phc.validateCard("2H"))
    Assert.assertEquals(false, phc.validateCard("2F"))
    Assert.assertEquals(false, phc.validateCard("G1"))

    // hand 1 - one pair; hand 2 - two pair
    Assert.assertEquals(2, phc.compareHands("AD KD KH JD TD","AD AH KH KD TD"))
    // hand 1 - two pair; hand 2 - better two pair
    Assert.assertEquals(2,phc.compareHands("5D 5S 2H 2D TD","5D 5H 3H 3D 2D"))
    // hand 1 - one pair better 3rd kicker; hand 2 - one pair
    Assert.assertEquals(1, phc.compareHands("5D 5S AH KD TD","5C 5H AD KH 2D"))
    // hand 1 - straight flush; hand 2 - quads
    Assert.assertEquals(1, phc.compareHands("2D 3D 4D 5D AD","6C 6H 6S 6D AS"))
    // hand 1 - flush; hand 2 - full house
    Assert.assertEquals(2, phc.compareHands("2C 3C KC QC AC","6C 6H 6S AD AS"))
    // hand 1 - ace high flush better kicker; hand 2 - ace high flush worse kicker
    Assert.assertEquals(1, phc.compareHands("2C TC KC QC AC","2H 3H KH QH AH"))
    // hand 1 - high card; hand 2 - high card better kicker
    Assert.assertEquals(2, phc.compareHands("6C TH JC 9C AC","6H TD QD 9D AH"))

  }
}
