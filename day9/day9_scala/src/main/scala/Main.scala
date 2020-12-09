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
        cartesianAutoProduct(l.init).filter{case (a, b) => a != b}
        .filter{case (a, b) => a + b == l.lastOption.getOrElse(0)}.isEmpty

    def challenge2(l: List[Int], targetNumber: Int): Option[Int] = {
        for (i <- 0 until l.length){
            for (j <- i until l.length){
                l.slice(i,j) match {
                    case m if m.sum == targetNumber => return Some(m.min + m.max)
                    case _ => None // discared anyway
                }
            }
        }
        None
    }

    val cypherInput = Source.fromFile("../input_day9.txt").getLines.flatMap(_.toIntOption).toList
    val firstMissingValue = cypherInput.sliding(26).filter(challenge1).toList.headOption.getOrElse(Nil).lastOption
    println(firstMissingValue.getOrElse(0)) // challenge 1
    println(challenge2(cypherInput, firstMissingValue.getOrElse(0))) // challenge 2
}