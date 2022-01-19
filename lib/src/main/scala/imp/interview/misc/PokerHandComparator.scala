package imp.interview.misc

import scala.annotation.tailrec
/*
  Problem:
  Given two strings representing a poker hand each, return the winning hand and log the summary.
  'Poker Hand' input example: 2D KH TS AC AD
  Author: Ivan Malyuta; 01/18/2022
 */
case class Card(name: Char, suit: Char, value: Int)
case class HandComponent(name: String, value: Int)
case class HandValuation(name: String, handComponents: List[HandComponent], kickers: List[Card])

class PokerHandComparator {
  val cardRanks = Map('2' -> 2, '3' -> 3, '4' -> 4, '5' -> 5, '6' -> 6, '7' -> 7, '8' -> 8, '9' -> 9, 'T' -> 10, 'J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14)
  val suits = List('H', 'D', 'C', 'S')
  val handRanks = Map("High Card"->1,"Pair"->2, "Two Pair"->3, "Trips"->4, "Straight"->5, "Flush"->6, "Full House"->7, "Quads"->8, "Straight Flush"->9)

  //case class HandValueDetails (hc1: Option[HandComponent], hc2: Option[HandComponent], kickers: List[Card])

  def validateCard(card: String): Boolean = {
    val z: Seq[Char] = card
    z match {
      case Seq(value@_, suit@_) => if (cardRanks.contains(value) && suits.contains(suit)) true else false
      case _ => false
    }
  }

  def getCard(card: String): Option[Card] = {
    if (validateCard(card))
      Some(Card(card.charAt(0), card.charAt(1), cardRanks(card.charAt(0))))
    else None
  }

  def getHandRank(hand: HandValuation):Int = {
    handRanks.getOrElse(hand.name,0)*1000 +
      (if (hand.handComponents.size > 0) hand.handComponents(0).value else 0)*100 +
      (if (hand.handComponents.size > 1) hand.handComponents(1).value else 0)*10
  }

  def getHand(cards: String): Option[List[Card]] = {
    val uCards: Array[String] = cards.split(" ")
    val cCards = uCards.map(c => getCard(c))
    val fSet = cCards.flatten.toSet
    if (cCards.contains(None) || cCards.length != 5 || fSet.size != 5)
      None
    else
      Some(fSet.toList)
  }

  def sortHand(hand: List[Card], handValues: Map[Char,Int]): List[Card] = {
    hand.sortBy(c=>handValues(c.name))
  }

  def compareHands(h1_str: String, h2_str: String): Int = {
    val h1 = getHand(h1_str)
    val h2 = getHand(h2_str)

    if (h1.isEmpty) {
      println("Hand 1 is invalid")
      return -1
    }
    if (h2.isEmpty) {
      println("Hand 2 is invalid")
      return -1
    }

    val hv1 = evaluateHand(h1.get)
    val hv2 = evaluateHand(h2.get)
    val rh1 = getHandRank(hv1)
    val rh2 = getHandRank(hv2)

    if (rh1 > rh2) {
      println("Hand 1 [" + hv1 + "] beats Hand 2 [" + hv2 + "]")
      1
    } else if (rh1 < rh2) {
      println("Hand 2 [" + hv2 + "] beats Hand 1 [" + hv1 + "]")
      2
    } else {
      // Run through kickers
      for ( (k1,k2) <- hv1.kickers zip hv2.kickers )
        if (k1.value > k2.value) {
          println("Hand 1 [" + hv1 + "] beats Hand 2 [" + hv2 + "] by kicker " + k1)
          return 1
        } else if (k2.value > k1.value) {
          println("Hand 2 [" + hv2 + "] beats Hand 1 [" + hv1 + "] by kicker " + k2)
          return 2
        }
      // Kickers did not resolve the winner; Push
      println("PUSH")
      0
    }

  }

  def evaluateHand(hand: List[Card]): HandValuation = {
    val res1 = evaluateHand1(hand);
    val hcs1 = res1._1
    val kickers = res1._2
    val shcs1 = hcs1.sortBy(a=> handRanks.get(a.name)).reverse
    // Flatten results from res1
    shcs1 match {
      case HandComponent("Pair",_) :: HandComponent("Pair",_) :: _ => HandValuation("Two Pair",List(shcs1(0), shcs1(1)),kickers)
      case HandComponent("Trips",_) :: HandComponent("Pair",_) :: _ => HandValuation("Full House",List(shcs1(0), shcs1(1)),kickers)
      case HandComponent(_,_) :: _ => HandValuation(shcs1(0).name, List(shcs1(0)),kickers)
      case _ =>
         evaluateHand2(kickers) match {
          case Some(hc) => HandValuation(hc.name, List(hc), kickers)
          case _ => HandValuation("High Card", List(), kickers)
        }
    }
  }

  // Check for Pairs, Trips, and Quads.
  def evaluateHand1(hand: List[Card]): (List[HandComponent], List[Card]) = {
    @tailrec
    def evaluateHandRec1(hand: List[Card], rvalue: Int, handComponents: List[HandComponent], kickers: List[Card]): (List[HandComponent],List[Card]) = hand match {
      case Nil => (handComponents, kickers)
      case head :: tail => {
        val sameValue = !tail.isEmpty && head.value == tail.head.value
        val hc = (sameValue) match {
          case true => None
          case false => (rvalue) match {
            case 2 => Some(HandComponent("Pair", head.value))
            case 3 => Some(HandComponent("Trips",head.value))
            case 4 => Some(HandComponent("Quads",head.value))
            case _ => None
          }
        }

        val nrvalue = (hc.isEmpty, sameValue) match {
          case (true, true) => rvalue + 1
          case (false, _) => 1
          case (_, _) => rvalue
        }

        val nHandComponents = if (hc.isEmpty) handComponents else hc.get :: handComponents
        val nKickers = if (!sameValue && hc.isEmpty) head :: kickers else kickers
        evaluateHandRec1(tail, nrvalue, nHandComponents, nKickers)
      }
    }
    evaluateHandRec1(hand.sortBy(a=>a.value), 1, List(), List())
  }

  // Check for Flush & Straight (with alternate Ace values)
  def evaluateHand2(hand: List[Card]): Option[HandComponent] = {
    @tailrec
    def evaluateHandRec2(hand: List[Card], rseq: Int, rsuit: Int, handComponent: Option[HandComponent]): Option[HandComponent] = hand match {
      case Nil => handComponent
      case head :: tail => {
        val nrseq = if (!tail.isEmpty && head.value == (tail.head.value-1)) rseq + 1 else rseq
        val nrsuit = if (!tail.isEmpty && head.suit == tail.head.suit) rsuit + 1 else rsuit
        val nHandComponent = (tail.isEmpty,nrseq,nrsuit) match {
          case (true,5,5) => Some(HandComponent("Straight Flush", head.value))
          case (true,5,_) => Some(HandComponent("Straight", head.value))
          case (true,_,5) => Some(HandComponent("Flush", head.value))
          case (_,_,_) => None
        }
        evaluateHandRec2(tail, nrseq, nrsuit, nHandComponent)
      }
    }

    val shand = hand.sortBy(a=>a.value)
    val res1 = evaluateHandRec2(shand, 1, 1, None)
    val last = shand.last
    if (last.name == 'A') {
      val res2 = evaluateHandRec2(Card('A', last.suit, 1) :: shand.dropRight(1), 1, 1, None)
      (res1, res2) match {
        case (Some(x), Some(y)) => Some(if (handRanks.get(x.name).get > handRanks.get(y.name).get) x else y)
        case (x, y) => x orElse y
      }
    }
    else
      res1
  }
}