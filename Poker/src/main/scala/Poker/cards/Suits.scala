package Poker.cards

sealed case class Suits (prioritySuits: Int) {

    def comparison(s2: Suits): Int = {
      if (this.prioritySuits > s2.prioritySuits)
        1
      else if (this.prioritySuits < s2.prioritySuits)
        -1
      else 0
    }

    def > (s2: Suits): Boolean = {
      comparison(s2) == 1
    }

    def < (s2: Suits): Boolean = {
      comparison(s2) == -1
    }

    def == (s2: Suits): Boolean = {
      comparison(s2) == 0
  }
}

case object Diamond extends Suits(1)
case object Heart extends Suits(2)
case object Club extends Suits(3)
case object Spade extends Suits(4)




