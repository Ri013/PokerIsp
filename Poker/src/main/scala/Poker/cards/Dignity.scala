package Poker.cards

sealed abstract case class Dignity(priorityDignity: Int){

 def comparison (d2: Dignity): Int = {
  if (this.priorityDignity > d2.priorityDignity)
   1
  else if (this.priorityDignity < d2.priorityDignity)
   - 1
  else 0
 }

 def > (d2: Dignity): Boolean = {
  comparison (d2) == 1
 }

 def < (d2: Dignity): Boolean = {
  comparison (d2) == - 1
 }

 def == (i: Int): Boolean = {
  this.priorityDignity == i
 }

 def  == (d2: Dignity): Boolean = {
  comparison(d2) == 0
 }

 def + (i: Int): Int ={
  this.priorityDignity + i
 }
}

case object Two extends Dignity(2)
case object Three extends Dignity(3)
case object Four extends Dignity(4)
case object Five extends Dignity(5)
case object Six extends Dignity(6)
case object Seven extends Dignity(7)
case object Eight extends Dignity(8)
case object Nine extends Dignity(9)
case object Ten extends Dignity(10)
case object Jack extends Dignity(11)
case object Queen extends Dignity(12)
case object King extends Dignity(13)
case object Ace extends Dignity(14)








