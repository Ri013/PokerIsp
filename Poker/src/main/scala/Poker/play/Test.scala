package Poker.play

import Poker.cards.{Card, Dignity, Suits}
import Poker.comb.Combination
import Poker.comb.Combination._


object Test extends App {
  val hand1 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Three")),
    Card(Suits("Diamond"), Dignity("Four"))))

  val hand2 = Hand(Vector(
    Card(Suits("Club"), Dignity("Six")),
    Card(Suits("Spade"), Dignity("Nine"))))

  val table2 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Ace")),
    Card(Suits("Club"), Dignity("Three")),
    Card(Suits("Spade"), Dignity("Seven")),
    Card(Suits("Diamond"), Dignity("Seven")),
    Card(Suits("Heart"), Dignity("Three"))))

  val table = Table(Vector(
    Card(Suits("Diamond"), Dignity("Jack")),
    Card(Suits("Club"), Dignity("Three")),
    Card(Suits("Spade"), Dignity("Three")),
    Card(Suits("Diamond"), Dignity("Seven")),
    Card(Suits("Heart"), Dignity("Three"))))

  val player2 = Player("Tom", hand2, table2)
  val player1 = Player("Rim", hand1, table)

  println(player2.comb.cardsPlayer.sorted(ordSuit))
  println(player2.comb.nameCombination)
  println(Combination.thisFullHouse(player1.comb.cardsPlayer))
  println(player1.comb.cardsPlayer.sorted(ordDignity))
  println(player1.comb.nameCombination)
}
//val prioritySuits = Map (
//"Diamond" -> 1,
//"Heart" -> 2,
//"Club" -> 3,
//"Spade" -> 4
//)Vector("Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace")