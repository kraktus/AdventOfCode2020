import scala.io.Source
import scala.util.matching.Regex
// day 4

object Main extends App {

	def splitByPassport(l: List[String], acc: String = ""): List[String] = l match {
		case "" :: tail =>  acc :: splitByPassport(tail)
		case head :: tail => splitByPassport(tail, acc + " " + head) //at least one space between two keys
		case _ => List(acc)
	}

	def isPassport(potentialPassport: String): Boolean = 
		Vector("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
		.forall(potentialPassport contains _)

	def between(x: String, lowerBound: Int, upperBound: Int): Boolean = x.toIntOption.exists(n => lowerBound <= n && n <= upperBound)

	def arePassportDatesOk(potentialPassport: String): Boolean = 
		Vector(("byr:(\\d{4})".r, 1920, 2002), ("iyr:(\\d{4})".r, 2010, 2020), ("eyr:(\\d{4})".r, 2020, 2030)) // regex and lower + upper bound
		.forall { case (rex, lowerBound, upperBound) => (rex findFirstMatchIn potentialPassport)
		.exists((m: Regex.Match) => between(m.group(1), lowerBound, upperBound)) 
		}

	def isHeightOK(potentialPassport: String): Boolean =
		("hgt:(\\d{2,3})(cm|in)".r findFirstMatchIn potentialPassport).exists((m: Regex.Match) => (m.group(2) == "cm" && between(m.group(1), 150, 193)) || (m.group(2) == "in" && between(m.group(1), 59, 76)))

	def isTruePassport(potentialPassport: String): Boolean =
		(Vector("hcl:#(\\d|[abcdef]){6}".r, "ecl:(amb|blu|brn|gry|grn|hzl|oth)".r, "pid:\\d{9}".r) // validity can be checked with regex only
		.forall((x: Regex) => (x findFirstIn potentialPassport).isDefined) &&
		arePassportDatesOk(potentialPassport)
 		&& isHeightOK(potentialPassport))

	val rawPassportInput = Source.fromFile("../input_day4.txt").getLines.toList
	val passportInput = splitByPassport(rawPassportInput)
	println(passportInput count isPassport) // challenge 1
	// not completely operational, finds 187 when it's 186 for my input
	println(passportInput count isTruePassport) // challenge 2
}