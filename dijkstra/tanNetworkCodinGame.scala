import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
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
        raw(0), raw(1).replaceAll("\"", ""),
        Coord(raw(3).toDouble, raw(4).toDouble),
    ))
    val edges = routesRaw.map(raw => Edge(raw(0), raw(1)))
    //Console.err.println(stops.mkString(", "))
    //Console.err.println(edges.mkString(", "))
    //Console.err.println(s"$startPoint -> $endPoint")
    //Console.err.println("---")
    
    // Write an answer using println
    // To debug: Console.err.println("Debug messages...")
    
    // EXPECTED: Route with stops on each line or "IMPOSSIBLE"
    Dijkstra.main(startPoint, endPoint, stops, edges)
}

object Dijkstra {


    def main(start: String, end: String, stops: Seq[Stop], edges: Seq[Edge]) = {
        val initialStop = stops.filter(s => s.id == start)(0)

        // 1. Select starting node, set its distance to 0. Set every other node distance to infinity
        var verticesFromStart = stops.map(stop => stop.id -> Vertex.init(stop)).to(collection.mutable.SortedMap)
        verticesFromStart(start) = verticesFromStart(start).copy(
            distanceFromInitial = 0,
            pathFromInitial = Seq(start))
        //println(s"Vertices Map = ${verticesFromStart.toString}")
       
        // TODO WIP
        verticesFromStart = dijkstra(start, edges, verticesFromStart)
        //println("Vertices: ")
        //verticesFromStart.foreach(v => println(s"- $v"))

        printRouteFromStartToEnd(end, verticesFromStart)
    }

    def dijkstra(currentId: String, edges: Seq[Edge], vertices: collection.mutable.SortedMap[String, Vertex]): collection.mutable.SortedMap[String, Vertex] = {
        // 2. Set the current node as the one with the smallest distance to the initial node
        val currentVertex: Vertex = vertices(currentId) 
        //println(s"Current vertex: $currentVertex")
        val neighboursIds: Seq[String] = edges.collect {
        case Edge(currentVertex.id, n) => n
        case Edge(n, currentVertex.id) => n
        }
        //Console.err.println(s"neighboursIds : $neighboursIds")
        val nonVisitedNeighbours = neighboursIds.filter(id => !vertices(id).visited)
        //Console.err.println(s"nonVisitedNeighbours : ${nonVisitedNeighbours.toString}")

       
        // 3. For each non-visited neighbour 
        nonVisitedNeighbours.foreach(neighbourId => {
            var neighbour = vertices(neighbourId)
            //     1. add the current distance with the edge's weight
            val distanceToCurrent = currentVertex.distance(neighbour)
            val newDistance = currentVertex.distanceFromInitial + distanceToCurrent
            //     2. If it's smaller to the neighbour's distance to the initial node, replace it
            if(newDistance < neighbour.distanceFromInitial) {
                neighbour = neighbour.copy(
                    distanceFromInitial = newDistance,
                    pathFromInitial = currentVertex.pathFromInitial ++ Seq(neighbour.id)
                )
                vertices(neighbourId) = neighbour
                //println(s"- neighbour: $neighbour")
            }
        })
        // 4. Mark the current node as visited
        vertices(currentVertex.id) = currentVertex.copy(visited = true)
        //println(s"currentVertex after visit: ${vertices(currentVertex.id).id} ${vertices(currentVertex.id)}")
        // 5. Repeat for non-visited nodes
        val nonVisitedVertices = vertices.filter(idVertex => !idVertex._2.visited)
        if(!nonVisitedVertices.isEmpty) {
            val closestNonVisitedVertices: collection.mutable.SortedMap[String, Vertex] = collection.mutable.SortedMap(nonVisitedVertices
                .toSeq
                .sortBy(_._2.distanceFromInitial):_*)
            val newCurrent = closestNonVisitedVertices.firstKey
            dijkstra(newCurrent, edges, vertices)
        }

        vertices
    }

    def printRouteFromStartToEnd(end: String, verticesFromStart: collection.mutable.SortedMap[String, Vertex]) = {
        val startToEndRoute = verticesFromStart(end).pathFromInitial
        startToEndRoute.foreach(stop => println(verticesFromStart(stop).stop.name))

    }

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

case class Vertex(id: String, distanceFromInitial: Double, pathFromInitial: Seq[String], visited: Boolean, stop: Stop) {
    def distance(other: Vertex) = this.stop.distance(other.stop)
}
object Vertex {
    def init(stop: Stop) = Vertex(stop.id, Double.PositiveInfinity, Seq.empty, false, stop)
}
