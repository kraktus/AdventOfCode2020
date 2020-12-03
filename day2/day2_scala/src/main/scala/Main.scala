import scala.util.matching.Regex
import scala.io.Source

object Main extends App {

	val listPwd = Source.fromFile("../input_day2.txt").getLines.toList
	val pattern = new Regex("^(\\d)+-(\\d)+ \\w")

	def checkIntegrityPwd_1(pwd: String): Boolean = {
		val List(lowerBound, upperBound, letter) = (pattern findFirstIn pwd).get.replace(" ", "-").split("-").toList
		// ex: "6-8 t: "
		val index = (lowerBound + upperBound + letter).length + 4
		val nbLetter = pwd.substring(index).count((x: Char) => x == letter.toCharArray.head)
		lowerBound.toInt <= nbLetter && nbLetter <= upperBound.toInt
	}

	def checkIntegrityPwd_2(pwd: String): Boolean = {
		val List(lowerBound, upperBound, letter) = (pattern findFirstIn pwd).get.replace(" ", "-").split("-").toList
		val index = (lowerBound + upperBound + letter).length + 4 // Ugly :/
		val letterChar = letter.toCharArray.head
		pwd.substring(index).zipWithIndex.foldLeft(false){
					(bool: Boolean, t: Tuple2[Char, Int]) => t match {
						case (x, i) if x == letterChar && (i+1 == lowerBound.toInt || i+1 == upperBound.toInt) => !bool 
						case _ => bool
						}
		}
	}

  println(listPwd.count(checkIntegrityPwd_1))
  println(listPwd.count(checkIntegrityPwd_2))
}