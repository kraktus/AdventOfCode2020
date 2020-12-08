import scala.io.Source

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

// day 7

// *not* functional

// Reminder: https://docs.scala-lang.org/cheatsheets/

object Main extends App {

    def pp[A](a: A): A = {println(a); a}

    // directed edge from the contained bag to the container bag
    def bagsEdges(line: String): List[DiEdge[String]] = {
        val containerBagRegex = "(.*) bags contain".r
        val containerBag = containerBagRegex.findFirstMatchIn(line) match {
            case Some(m) => m.group(1)
            case _ => "unknown"
        }
        //println(containerBag)
        val containedBagsRegex = "\\d (\\w* \\w*) bag".r
        containedBagsRegex.findAllMatchIn(line).map(_.group(1) ~> containerBag).toList
    }

    //There's probably already a function that does that
    def getNbContainers(nodes: Set[Main.bagsGraph.NodeT], alreadySearched: Set[Main.bagsGraph.NodeT] = Set()): Int =
    nodes.map(_.diSuccessors).reduce(_ union _) match {
        case x if x.size == 0 => pp(nodes).size
        case x => pp(nodes).size + getNbContainers(x.filterNot(alreadySearched.contains(_)), x union alreadySearched)
    } 

    val bagsInput = Source.fromFile("../small_input2.txt").getLines.toList
    val bagsGraph = Graph.from(List(), bagsInput flatMap bagsEdges)
    println(getNbContainers((bagsGraph get "shiny gold").diSuccessors, Set(bagsGraph get "shiny gold")))

    println(bagsGraph)
}