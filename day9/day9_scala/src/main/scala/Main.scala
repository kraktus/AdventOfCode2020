import scala.io.Source

// day 9

// Reminder: https://docs.scala-lang.org/cheatsheets/

object Main extends App {

    def cartesianAutoProduct[A](l: Iterable[A]): Iterable[(A,A)] = {
        for {
            nb1 <- l
            nb2 <- l
            } yield (nb1, nb2)
    }

    def challenge1(l: List[Int]): Boolean = 
        cartesianAutoProduct(l.init).filter{case (a, b) => a + b == l.lastOption.getOrElse(0) && a != b}.isEmpty

    def challenge2(l: List[Int], targetNumber: Int, lowerBound: Int = 0, upperBound: Int = 1): Option[Int] = {
        val slicedList =  l.slice(lowerBound,upperBound)
        slicedList.sum match {
            case m if m == targetNumber => return Some(slicedList.min + slicedList.max)
            case m if m < targetNumber => challenge2(l, targetNumber, lowerBound, upperBound + 1)
            case _ => challenge2(l, targetNumber, lowerBound + 1, upperBound) // m > targetNumber
        }
    }

    val cypherInput = Source.fromFile("../input_day9.txt").getLines.flatMap(_.toIntOption).toList
    val firstMissingValue = cypherInput.sliding(26).filter(challenge1).toList.headOption.getOrElse(Nil).lastOption
    println(firstMissingValue.getOrElse(0)) // challenge 1
    println(challenge2(cypherInput, firstMissingValue.getOrElse(0))) // challenge 2
}