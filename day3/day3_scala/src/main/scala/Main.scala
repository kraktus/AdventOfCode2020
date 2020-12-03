import scala.io.Source

// day 3

object Main extends App {

	class Matrix(
		private val matrixVec: Vector[String],
		private val row: Int,
		private val column: Int
	) {

		def check_sanity: Boolean =
			matrixVec.foldLeft(true)(
				(b: Boolean, a: String) => b match {
					case true => a.length == column
					case _ => false
					}
			) && matrixVec.length == row

		def getRow = row
		def getColumn = column

		def get(row_input: Int, column_input: Int): Char =
			// never get out of range
			matrixVec(row_input % row).charAt(column_input % column)

	}

	def createMatrix(lines: Vector[String]): Matrix = {

		val matrixVec: Vector[String] = lines.map(
			_.filter((x: Char) => x == '#' || x == '.')
		)
		val row: Int = matrixVec.length
		val column: Int = matrixVec(0).length // unsafe for now
		new Matrix(matrixVec, row, column)
	}

	def countTree(matrix: Matrix, xIncr: Int, yIncr: Int): Int = {
		(0 until matrix.getRow by yIncr).map(
			(y: Int) => (y, y/yIncr * xIncr) // y, x
		)
		.foldLeft(0)(
			(acc: Int, coord: Tuple2[Int, Int]) => matrix.get(coord._1, coord._2) match {
				case '#' => acc + 1
				case _ => acc
			}
		)
	}

	def countTreeOnMultiplePath(matrix: Matrix): BigInt =
		// product is too big for Int
		List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
		.map {
			case (a, b) => countTree(matrix, a, b)
		}
		.map(BigInt(_))
		.product

	val mapInput = Source.fromFile("../input_day3.txt").getLines.toVector
	// val mapInput = Source.fromFile("../small_input.txt").getLines.toList
	val matrixToboggan = createMatrix(mapInput)
	if (matrixToboggan.check_sanity) {
		println(countTree(matrixToboggan, 3, 1)) // challenge 1
		println(countTreeOnMultiplePath(matrixToboggan)) // challenge 2
	} else {
		println("matrix is not sane")
	}
}