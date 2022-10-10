package Poker.play

import Poker.cards.{Card, Dignity, Suits}
import Poker.comb.Combination._

final case class Hand( cardsHand: Vector[Card]) {

  def comparison (p2: Hand): Int = {
    val h1 = this.cardsHand.sorted(ordDignity)
    val h2 = p2.cardsHand.sorted(ordDignity)
     if (h1(h1.length - 1).dignity > h2(h2.length - 1).dignity)
      1
    else if (h1(h1.length - 1).dignity < h2(h2.length - 1).dignity)
      - 1
    else if (h1(0).dignity > h2(0).dignity)
      1
    else if (h1(0).dignity < h2(0).dignity)
      -1
    else 0
  }

  def > (h2: Hand): Boolean = {
    comparison (h2) == 1
  }
  def < (h2: Hand): Boolean = {
    comparison (h2) == - 1
  }
}

