import scala.io.Source

// day 8

// Reminder: https://docs.scala-lang.org/cheatsheets/

object Main extends App {

    def pp[A](a: A): A = {
        println(a)
        a
    }

    def challenge1(lineIndex: Int, lines: Vector[String], acc: Int, linesAlreadyDone: Set[Int] = Set()): Int = {
        if (linesAlreadyDone.contains(lineIndex)) return acc
        lines(lineIndex) match {
            case s"nop ${dig}" => challenge1(lineIndex + 1, lines, acc, linesAlreadyDone + lineIndex)
            case s"acc ${dig}" => challenge1(lineIndex + 1, lines, acc + dig.toInt, linesAlreadyDone + lineIndex)
            case s"jmp ${dig}" => challenge1(lineIndex + dig.toInt, lines, acc, linesAlreadyDone + lineIndex)
            case _ => 0
        }
    }  

    def innerChallenge2(lineIndex: Int, lines: Vector[String], acc: Int, linesAlreadyDone: Set[Int] = Set()): Option[Int] = {
        if (linesAlreadyDone.contains(lineIndex)) return None
        else if (lineIndex == lines.length) return Some(acc)
        lines(lineIndex) match {
            case s"nop ${dig}" => innerChallenge2(lineIndex + 1, lines, acc, linesAlreadyDone + lineIndex)
            case s"acc ${dig}" => innerChallenge2(lineIndex + 1, lines, acc + dig.toInt, linesAlreadyDone + lineIndex)
            case s"jmp ${dig}" => innerChallenge2(lineIndex + dig.toInt, lines, acc, linesAlreadyDone + lineIndex)
            case _ => None
        }
    }

    def challenge2(lines: Vector[String], line: String, index: Int): Option[Int] = 
        line match {
            case s"nop ${dig}" => innerChallenge2(0, lines.updated(index, pp(s"jmp ${dig}")), 0)
            case s"jmp ${dig}" => innerChallenge2(0, lines.updated(index, pp(s"nop ${dig}")), 0)
            case _ => None
        }

    val cmdInput = Source.fromFile("../input_day8.txt").getLines.toVector
    println(challenge1(0, cmdInput, 0))
    println(cmdInput.zipWithIndex.flatMap {case (v,i) => challenge2(cmdInput, v, i)})
}