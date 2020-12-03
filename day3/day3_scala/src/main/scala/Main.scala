import scala.io.Source

// day 3

object Main extends App {

	class Matrix(
		private val matrixVec: Vector[String],
		val row: Int,
		val column: Int
	) {

		def get(rowInput: Int, columnInput: Int): Char =
			// never get out of range
			matrixVec(rowInput % row).charAt(columnInput % column)
	}

	def createMatrix(lines: Vector[String]): Option[Matrix] = {

		val matrixVec: Vector[String] = lines.map(
			_.filter((x: Char) => x == '#' || x == '.')
		)
		val row: Int = matrixVec.length
		val column: Int = matrixVec(0).length
		if (matrixVec.forall(_.length == column) && matrixVec.length == row)
			Some(new Matrix(matrixVec, row, column))
		else {
			None
		}
	}

	def countTree(matrix: Matrix, xIncr: Int, yIncr: Int): Int = {
		(0 until matrix.row by yIncr).map(
			(y: Int) => (y, y/yIncr * xIncr) // y, x
		)
		.count {
			case (y, x) => matrix.get(y, x) == '#'
		}
	}

	def countTreeOnMultiplePath(matrix: Matrix): BigInt =
		// product is too big for Int
		List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
		.map {
			case (a, b) => countTree(matrix, a, b)
		}
		.map(_.toLong)
		.product

	val mapInput = Source.fromFile("../input_day3.txt").getLines.toVector
	// val mapInput = Source.fromFile("../small_input.txt").getLines.toList
	createMatrix(mapInput) match {
		case Some(matrix) => {
			println(countTree(matrix, 3, 1)) // challenge 1
			println(countTreeOnMultiplePath(matrix)) // challenge 2
			}
		case _ => println("input is not sane")
		}
}