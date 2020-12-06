import scala.io.Source
import scala.util.matching.Regex
// day 6

object Main extends App {

	def dummyUnion(a: Set[Char], b: Set[Char]): Set[Char] = a union b
	def dummyIntersection(a: Set[Char], b: Set[Char]): Set[Char] = a intersect b

	def splitByPersonAndCount(l: List[String], acc: Set[Char], op: (Set[Char], Set[Char]) => Set[Char]): Int = l match {
		case "" :: Nil => acc.size
	 	case "" :: head :: tail => acc.size + splitByPerson(tail, head.toSet, op)
	 	case head :: tail => splitByPerson(tail, op(acc,head.toSet), op)
	 	case _ => acc.size
	}

	val questionsInput = Source.fromFile("../input_day6.txt").getLines.toList
	println(splitByPersonAndCount(questionsInput.tail, questionsInput.head.toSet, dummyUnion)) //challenge 1
	println(splitByPersonAndCount(questionsInput.tail, questionsInput.head.toSet, dummyIntersection)) //challenge 2
}