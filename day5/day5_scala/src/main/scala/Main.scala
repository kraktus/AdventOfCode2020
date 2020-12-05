import scala.io.Source
import scala.util.matching.Regex
// day 5

object Main extends App {

    case class Seat(row: Int, column: Int)

    def readFile(line: String): Option[Seat] = {
        line.toList.splitAt(7) match {
            case (rowPart, columnPart) =>
                for {
                    row <- dichotomy(rowPart, 0, 127)
                    column <- dichotomy(columnPart, 0, 7)
                } yield Seat(row, column)
            case _ => None
        }
    }
    // a classic
    def dichotomy(input: List[Char], lowerBound: Int, upperBound: Int): Option[Int] = 
        input match {
            case ('R'|'B') :: Nil => Some(upperBound)
            case ('F'|'L') :: Nil => Some(lowerBound)
            case ('R'|'B') :: tail => dichotomy(tail, (lowerBound+upperBound)/2+1, upperBound)
            case ('F'|'L') :: tail => dichotomy(tail, lowerBound, (lowerBound+upperBound)/2)
            case _ => None
        }

    def missingSeat(sortedSeatIds: List[Int]): Option[Int] = {
        sortedSeatIds match {
            case x :: y :: tail if y == x + 1 => missingSeat(y :: tail)
            case x :: y :: _ if y == x + 2 => Some(x+1)
            case _ => None
        }
    }

    val input = Source.fromFile("../input_day5.txt").getLines.toList
    val seatList: List[Seat] = input flatMap readFile
    val seatIds: List[Int] = seatList.map {case Seat(y, x) => y*8 + x}
    println(seatIds.max) // challenge 1
    val sortedSeatIds = seatIds.sorted
    println(missingSeat(sortedSeatIds)) // challenge 2

}