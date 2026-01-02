import math._
import scala.util._
import scala.io.StdIn._

/**
 * https://www.codingame.com/ide/puzzle/tan-network
 *
 * TODO : 
 * - rewrite the vertices use to keep a mutable list, with initial values at infinite distance
 * - Implement looking at every vertex or put recursivity
 * etc
 **/
object Solution extends App {
    val startPoint = readLine.split(':')(1)
    val endPoint = readLine.split(':')(1)
    val stopsCount = readLine.toInt
    val stopsRaw = new Array[Array[String]](stopsCount)
    for(i <- 0 until stopsCount) {
        val stopName = readLine.split(':')(1).split(',')
        stopsRaw(i) = stopName
        //println(stopName.mkString(", "))
    }
    //println(stopsRaw.map(r => r.mkString(", ")).mkString(" | "))

    val routesCount = readLine.toInt
    val routesRaw = new Array[Array[String]](routesCount)
    for(i <- 0 until routesCount) {
        val route = readLine.split(' ').map(stop => stop.split(':')(1))
        routesRaw(i) = route
        //println(route.mkString(", "))
    }    
    //println(routesRaw.map(r => r.mkString("->")).mkString(" | "))

    //
    // SETUP
    //
    val stops = stopsRaw.map(raw => Stop(
        raw(0), raw(1),
        Coord(raw(3).toDouble, raw(4).toDouble),
    ))
    val edges = routesRaw.map(raw => Edge(raw(0), raw(1)))
    println(stops.mkString(", "))
    println(edges.mkString(", "))
    println(s"$startPoint -> $endPoint")
    println("---")
    
    // Write an answer using println
    // To debug: Console.err.println("Debug messages...")
    
    // EXPECTED: Route with stops on each line or "IMPOSSIBLE"
    Dijkstra.main(startPoint, endPoint, stops, edges)
}

object Dijkstra {


    def main(start: String, end: String, stops: Seq[Stop], edges: Seq[Edge]) = {
        val initialStop = stops.filter(s => s.id == start)(0)

        // 1. Select starting node, set its distance to 0. Set every other node distance to infinity
        val initialVertex = Vertex(start, 0, Seq(start))
        var vertices = Seq(initialVertex)
       
        // 2. Set the current node as the one with the smallest distance to the initial node
        // 3. For each non-visited neighbour 
        //     1. add the current distance with the edge's weight
        //     2. If it's smaller to the neighbour's distance to the initial node, replace it
        // 4. Mark the current node as visited
        // 5. Repeat for non-visited nodes

        // INITIAL NODE NEIGHBOURS
        val currentStop = start
        val neighboursAsVertices = edges.filter(edge => edge.startId == currentStop)
            .map(edge => edge.endId)
            .map(neighbourId => {
                val neighbour = stops.filter(s => s.id == neighbourId)(0)
                val distance = initialStop.distance(neighbour)
                val path = initialVertex.pathFromInitial ++ Seq(neighbourId)
                // TODO Path update if new distance inferior
                Vertex(neighbourId, distance, path)
            })
        vertices = vertices ++ neighboursAsVertices

        for {
            currentNode <- neighboursAsVertices
            neighbourId <- edges.filter(edge => edge.startId == currentNode.id).map(_.endId).filter(id => !vertices.exists(v => v.id == id))
        } yield ()
        
        println(vertices.mkString(", "))
        println("WIP")
    }

    def ???()

}

case class Coord(latitude: Double, longitude: Double) {
    def distance(other: Coord): Double = {
        val c = Math.cos((this.latitude + other.latitude) / 2)
        val x = (other.longitude - this.longitude) * c
        val y = other.latitude - this.latitude
        Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2)) * 6371
    }
}
case class Stop(id: String, name: String, coord: Coord) {
    def distance(other: Stop): Double = this.coord.distance(other.coord)
}
case class Edge(startId: String, endId: String)

case class Vertex(id: String, distanceFromInitial: Double, pathFromInitial: Seq[String])
object Vertex {
    def init(id: String) = Vertex(id, Double.PositiveInfinity, Seq.empty)
}
