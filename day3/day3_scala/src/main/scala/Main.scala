import scala.io.Source
//import scala.math.BigInt

// day 3

object Main extends App {

	class Matrix(private var row: Int = 0, private var column: Int = 0) {

		// |
		// |
		// |
		// 

		private var matrixArrays = Array.empty[String]

		def addRow(line: String): Unit = {

			val lineFiltered = line.filter((x: Char) => x == '#' || x == '.')
			column = lineFiltered.length
			matrixArrays = matrixArrays :+ lineFiltered
			row += 1
		}

		def check_sanity: Boolean =
			matrixArrays.foldLeft(true)(
				(b: Boolean, a: String) => b match {
					case true => a.length == column
					case _ => false
					}
			) && matrixArrays.length == row

		def getRow = row
		def getColumn = column

		def get(row_input: Int, column_input: Int): Char =
			// never get out of range
			matrixArrays(row_input % row).charAt(column_input % column)

	}

	def countTree(matrix: Matrix, xIncr: Int, yIncr: Int): Int = {
		var acc = 0
		var x = 0
		var y = 0
		while (y < matrix.getRow){
			matrix.get(y, x) match {
				case '#' => acc += 1
				case _ => ()
			}
			x += xIncr
			y += yIncr
			// println(y, y % matrix.getRow)
		}
		println(y,x)
		println(acc)
		acc
	}

	def countTreeOnMultiplePath(matrix: Matrix): BigInt =
		// product is too big for Int
		List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
		.map(
			(t: Tuple2[Int, Int]) =>
			countTree(matrix, t._1, t._2)
		)
		.map(BigInt(_))
		.product

	val mapInput = Source.fromFile("../input_day3.txt").getLines.toList
	// val mapInput = Source.fromFile("../small_input.txt").getLines.toList
	val matrixToboggan = new Matrix
	mapInput.foreach(matrixToboggan.addRow)
	if (matrixToboggan.check_sanity) {
		println(countTree(matrixToboggan, 3, 1)) // challenge 1
		println(countTreeOnMultiplePath(matrixToboggan)) // challenge 2
	} else {
		println("matrix is not sane")
	}
}