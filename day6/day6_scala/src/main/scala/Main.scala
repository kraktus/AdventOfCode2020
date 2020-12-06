import scala.io.Source
import scala.util.matching.Regex
// day 6

object Main extends App {

	def union[A](a: Set[A], b: Set[A]): Set[A] = a union b // calling the standard lib func
	def intersect[A](a: Set[A], b: Set[A]): Set[A] = a intersect b // calling the standard lib func

	def splitByPersonAndCount(l: List[String], acc: Set[Char], op: (Set[Char], Set[Char]) => Set[Char]): Int = l match {
		case "" :: Nil => acc.size
	 	case "" :: head :: tail => acc.size + splitByPersonAndCount(tail, head.toSet, op)
	 	case head :: tail => splitByPersonAndCount(tail, op(acc,head.toSet), op)
	 	case _ => acc.size
	}

	val questionsInput = Source.fromFile("../input_day6.txt").getLines.toList
	println(splitByPersonAndCount(questionsInput.tail, questionsInput.head.toSet, union)) //challenge 1
	println(splitByPersonAndCount(questionsInput.tail, questionsInput.head.toSet, intersect)) //challenge 2
}