package Poker.play

import Poker.cards.{Card, Dignity, Suits}
import Poker.comb.{Combination, ComparisonCombination}

import scala.util.Random

object testGame extends App{
  val hand1 = Hand(Vector(
    Card(Suits("Diamond"), Dignity ("Ten")),
    Card(Suits("Diamond"), Dignity ("Jack"))))
  val table1 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Queen")),
    Card(Suits("Diamond"), Dignity("King")),
    Card(Suits("Spade"), Dignity("Ace")),
    Card(Suits("Diamond"), Dignity("Ace")),
    Card(Suits("Heart"), Dignity("Ten"))))

  val royalFlush = Player ("RF", hand1, table1)

  println(royalFlush.comb.nameCombination)

  val hand2 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Ten")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table2 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Queen")),
    Card(Suits("Diamond"), Dignity("King")),
    Card(Suits("Spade"), Dignity("Ace")),
    Card(Suits("Diamond"), Dignity("Nine")),
    Card(Suits("Heart"), Dignity("Ten"))))

  val straightFlush = Player("SF", hand2, table2)

  println(straightFlush.comb.nameCombination)

  val hand3 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Ten")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table3 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Queen")),
    Card(Suits("Diamond"), Dignity("King")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("Ten")),
    Card(Suits("Heart"), Dignity("Ten"))))

  val care = Player("C", hand3, table3)

  println(care.comb.nameCombination)

  val hand4 = Hand(Vector(
    Card(Suits("Club"), Dignity("Jack")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table4 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Queen")),
    Card(Suits("Diamond"), Dignity("King")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("Ten")),
    Card(Suits("Heart"), Dignity("Ten"))))

  val fullHouse = Player("FH", hand4, table4)

  println(fullHouse.comb.nameCombination)

  val hand5 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Two")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table5 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Queen")),
    Card(Suits("Diamond"), Dignity("King")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Diamond"), Dignity("Three")),
    Card(Suits("Heart"), Dignity("Ten"))))

  val flush = Player("F", hand5, table5)

  println(flush.comb.nameCombination)

  val hand6 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Two")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table6 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Five")),
    Card(Suits("Diamond"), Dignity("Six")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("Three")),
    Card(Suits("Heart"), Dignity("Four"))))

  val straight = Player("S", hand6, table6)

  println(straight.comb.nameCombination)

  val hand7 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Two")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table7 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Five")),
    Card(Suits("Diamond"), Dignity("Six")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("Two")),
    Card(Suits("Heart"), Dignity("Two"))))

  val troika = Player("T", hand7, table7)

  println(troika.comb.nameCombination)

  val hand8 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Two")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table8 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Five")),
    Card(Suits("Diamond"), Dignity("Six")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("Two")),
    Card(Suits("Heart"), Dignity("Five"))))

  val twoPair = Player("TP", hand8, table8)

  println(twoPair.comb.nameCombination)

  val hand9 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Two")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table9 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Five")),
    Card(Suits("Diamond"), Dignity("Six")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("Two")),
    Card(Suits("Heart"), Dignity("Ace"))))

  val Pair = Player("P", hand9, table9)

  println(Pair.comb.nameCombination)
  val hand10 = Hand(Vector(
    Card(Suits("Diamond"), Dignity("Two")),
    Card(Suits("Diamond"), Dignity("Jack"))))
  val table10 = Table(Vector(
    Card(Suits("Diamond"), Dignity("Five")),
    Card(Suits("Diamond"), Dignity("Six")),
    Card(Suits("Spade"), Dignity("Ten")),
    Card(Suits("Club"), Dignity("King")),
    Card(Suits("Heart"), Dignity("Ace"))))

  val highCard = Player("HC", hand10, table10)

  println(highCard.comb.nameCombination)
//  val hand2 = Hand(Vector(Card(Suits("Club"), Dignity ("Six")), Card(Suits("Spade"), Dignity ("Nine"))))
//
//  val table = Table(Vector(Card(Suits("Diamond"), Dignity ("Jack")), Card(Suits("Diamond"), Dignity ("Ace")),
//    Card(Suits("Spade"), Dignity ("Ace")),Card(Suits("Diamond"), Dignity ("Seven")),Card(Suits("Heart"), Dignity ("Ten"))))
//
//
//  val player1 = Player("Rim", hand1, table)
//  val player2 = Player("Tom", hand2, table)
//  println(player1.comb)
//   println(player2.comb)
//  println(player1.comb.nameCombination)
//  println(player2.comb.nameCombination)
//println(ComparisonCombination.compaireVector(Vector(player1, player2)))
}

//val prioritySuits = Map (
//"Diamond" -> 1,
//"Heart" -> 2,
//"Club" -> 3,
//"Spade" -> 4
//)Vector("Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace")