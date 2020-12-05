import scala.io.Source
import scala.util.matching.Regex
// day 5

object Main extends App {

	case class Seat(row: Int, column: Int)

    def readFile(line: List[Char]): Option[Seat] = {
        line.splitAt(7) match {
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
			case head :: Nil if head == 'B' || head == 'R' => Some(upperBound)
			case head :: Nil if head == 'F' || head == 'L' => Some(lowerBound)
			case head :: tail if head == 'B' || head == 'R' => dichotomy(tail, (lowerBound+upperBound)/2+1, upperBound)
			case head :: tail if head == 'F' || head == 'L' => dichotomy(tail, lowerBound, (lowerBound+upperBound)/2)
			case _ => None
		}

	val input = Source.fromFile("../input_day5.txt").getLines.toList
	// can't use flatmap on List[List[Char]]
	val seatList: List[Seat] = input.map(_.toList).map(readFile _).flatten
	val seatIds: List[Int] = seatList.map {case Seat(y, x) => y*8 + x}
	println(seatIds.max) // challenge 1
	val sortedSeatIds = seatIds.sorted
	println((sortedSeatIds zip (sortedSeatIds.head to sortedSeatIds.length - sortedSeatIds.head) filter {case (a,b) => a != b}).head._2) // challenge 2
}