import java.io.{File, IOException}
import java.util.Scanner
import scala.util.Try
import scala.collection.mutable.{Map}
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

import scala.util.{Random, Try}
import scala.collection.mutable

import scala.util.control._

// create a Breaks object 

object graph
{
    /*
    A trait for representing directed and undirected graphs
    */
    trait Graph[T]
    {
        def isDirected:Boolean

        def getVertices:Iterable[T]

        def getEdges:Iterable[Edge[T]]

        def edgeExists(source:T, destination:T):Boolean

        def getEdgeWeight(source:T, destination:T):Option[Int]

        def getEdge(source:T, destination:T):Option[Edge[T]]

        @throws(classOf[IllegalArgumentException])
        def addVertex(vertex:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def removeVertex(vertex:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def addEdge(source:T, destination:T, weight:Int):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def removeEdge(source:T, destination:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def getAdjacent(source:T):Iterable[T]

        def pathLength(path:Seq[T]):Option[Long]

        @throws(classOf[IllegalArgumentException])
        def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]]

        def greedyTSP():Seq[Edge[T]]
        
        def greedyTSP(initialTour:Seq[T]):Seq[Edge[T]]

        def dynamicTSP:Seq[Edge[T]]

        override def toString:String
    }


    /*
     * A class that represents an edge with a source, destination and weight
     */
    class Edge[T](val source:T, val destination:T, val weight:Int) extends Ordered[Edge[T]]
    {
        override def toString:String = source + " -> " + destination + " (" + weight + ")"

        def compare(other:Edge[T]):Int =
        {
            if (this.weight < other.weight) -1
            else if (this.weight > other.weight) 1
            else 0
        }
    }
    

    /**
     * Serves as a factory function for producing new empty Graphs
     */
    object Graph
    {
        /*
         * Creates and returns a new empty Graph - acts as a constructor
         */
        def apply[T](isDirected: Boolean): Graph[T] =
        {
            val vertices: HashSet[T] = HashSet[T]()
            val edges: HashMap[(T, T), Int] = HashMap[(T, T), Int]()
            
            new GraphImpl(isDirected, vertices, edges)
        }


        /**
         * Create a graph from a CSV file
         * 
         * <# of vertices>
         * <first vertex>
         * <second vertex>
         * ...
         * <nth vertex>
         * <# of edges>
         * <source>,<destination>,<weight>
         * <source>,<destination>,<weight>
         */
        def fromCSVFile(isDirected:Boolean, fileName:String):Graph[String] =
        {
            try {
                // use scanner to read the file
                val scanner = new Scanner(new File(fileName))

                // read the number of vertices
                val numVertices = scanner.nextLine().toInt

                // read the vertices
                var vertices: HashSet[String] = HashSet[String]()

                for (i <- 0 until numVertices)
                {
                    vertices += scanner.nextLine()
                }

                // read the number of edges
                val numEdges = scanner.nextLine().toInt

                // read the edges
                val edges = (1 to numEdges).map(i =>
                {
                    val line = scanner.nextLine()
                    val tokens = line.split(",")
                    val source = tokens(0)
                    val destination = tokens(1)
                    val weight = tokens(2).toInt
                    ((source, destination), weight)
                }).toMap


                scanner.close()

                new GraphImpl(isDirected, vertices, HashMap[(String, String), Int]() ++ edges)
            }
            catch {
                case e:IOException => throw new IOException(`fileName` + " cannot be processed")
            }
        }


        /**
         * Creates a private graph implementation class that extends Graph and
         * implements the Graph interface
         *
         * @tparam T the type of the vertices
         * @author hzhu20@georgefox.edu
         */
        private class GraphImpl[T] (
            val isDirected:Boolean,
            val vertices: HashSet[T],
            val edges: HashMap[(T, T), Int]
        ) extends Graph[T]
        {
            /**
             * Returns the vertices of the graph
             * @return the list of vertices in the graph
             */
            def getVertices:Iterable[T] = vertices

            /**
             * Gets edges from the graph
             * @return the list of edges in the graph
             */
            def getEdges:Iterable[Edge[T]] =
            {
                var edges: HashSet[Edge[T]] = HashSet[Edge[T]]()

                for (((source, destination), weight) <- this.edges)
                {
                    if (isDirected)
                    {
                        edges += new Edge(source, destination, weight)
                    }
                    else
                    {
                        edges += new Edge(source, destination, weight)
                        edges += new Edge(destination, source, weight)
                    }
                }

                edges
            }

            def getEdge(source:T, destination:T):Option[Edge[T]] = {

                // get the edge from the edges list
                val edge = this.edges.get((source, destination))

                // if the edge exists, return it
                if (edge.isDefined)
                {
                    Some(new Edge(source, destination, edge.get))
                }
                else
                {
                    None
                }
            }


            /**
             * Returns true if an edge exists between the two given vertices
             * @return true if an edge exists between the two given vertices
             */
            def edgeExists(source:T, destination:T):Boolean = {
                if (!isDirected) this.edges.contains((source, destination)) || this.edges.contains((destination, source))
                else this.edges.contains((source, destination))
            }


            /**
             * Returns the weight of the edge between the two given vertices
             * @return the weight of the edge between the two given vertices
             */
            def getEdgeWeight(source:T, destination:T):Option[Int] = {
                
                // count each edge once even if it's undirected
                val edge = (source, destination)
                val reverseEdge = (destination, source)

                if (edges.contains(edge)) Some(edges(edge))
                else if (edges.contains(reverseEdge)) Some(edges(reverseEdge))
                else None
            }


            /**
             * Adds the given vertex to the graph
             *
             * @param vertex the vertex to add
             * @return the graph with the given vertex added
             */
            def addVertex(vertex:T):Graph[T] = {

                if (vertices.contains(vertex))
                {
                    throw new IllegalArgumentException("Vertex already exists")
                }
                else if (vertex == null)
                {
                    throw new IllegalArgumentException("Vertex can't be null!")
                }
                else
                {
                    val newVertices = vertices + vertex
                    val newEdges = edges
                    new GraphImpl(isDirected, newVertices, newEdges)
                }
            }


            /**
             * Removes the given vertex from the graph
             *
             * @param vertex the vertex to remove
             * @return the graph with the given vertex removed
             */
            def removeVertex(vertex:T):Graph[T] = {

                if (vertex == null)
                {
                    throw new IllegalArgumentException("Vertex can't be null!")
                }

                if (vertices.contains(vertex))
                {
                    val newVertices = vertices - vertex
                    val newEdges = edges.filter(edge => !(edge._1._1 == vertex || edge._1._2 == vertex))
                    new GraphImpl(isDirected, newVertices, newEdges)
                }
                else
                {
                    throw new IllegalArgumentException("Vertex doesn't exist")
                }
            }


            /**
             * Adds an edge to the graph
             *
             * @param source the source vertex
             * @param destination the destination vertex
             * @param weight the weight of the edge
             * @return the graph with the edge added
             */
            def addEdge(source:T, destination:T, weight:Int):Graph[T] = {
                if (source == null && destination == null)
                {
                    throw new IllegalArgumentException("Null arguments not allowed")
                }

                if (vertices.contains(source) && vertices.contains(destination))
                {
                    if (edgeExists(source, destination))
                    {
                        throw new IllegalArgumentException("Edge already exists")
                    }
                    else if (source == destination)
                    {
                        throw new IllegalArgumentException("Loops are not allowed")
                    }
                    else
                    {
                        if (isDirected)
                        {
                            val newEdges = edges + ((source, destination) -> weight)
                            new GraphImpl(isDirected, vertices, newEdges)
                        }
                        else
                        {
                            val newEdges = edges + ((source, destination) -> weight)
                            val newEdges2 = edges + ((destination, source) -> weight)
                            new GraphImpl(isDirected, vertices, newEdges ++ newEdges2)
                        }
                    }
                }
                else
                {
                    throw new IllegalArgumentException("One or both vertices do not exist")
                }
            }


            /**
             * Removes the edge between the two given vertices
             *
             * @param source the source vertex
             * @param destination the destination vertex
             * @return the graph with the edge removed
             */
            def removeEdge(source:T, destination:T):Graph[T] = {

                if (vertices.contains(source) && vertices.contains(destination))
                {
                    if (edgeExists(source, destination))
                    {
                        val newEdges = edges - ((source, destination))
                        if (isDirected)
                        {
                            new GraphImpl(isDirected, vertices, newEdges)
                        }
                        else
                        {
                            val newEdges2 = edges - ((destination, source))
                            new GraphImpl(isDirected, vertices, newEdges ++ newEdges2)
                        }
                    }
                    else
                    {
                        throw new IllegalArgumentException("Edge doesn't exist")
                    }
                }
                else
                {
                    throw new IllegalArgumentException("One or both vertices do not exist")
                }
            }


            /**
             * Get adjacent vertices of the source vertex given
             */
            def getAdjacent(source:T):Iterable[T] = {
                
                if (!vertices.contains(source))
                {
                    throw new IllegalArgumentException("Vertex does not exist")
                }

                edges.filter(e => e._1._1 == source).map(e => e._1._2)
            }


            /*
             * Returns length of path given a list of vertices
             * or None if no path exists
             */
            def pathLength(path: Seq[T]): Option[Long] = {
                var length = 0L
                path.toVector.sliding(2).foreach(pair => {
                    length += getEdgeWeight(pair.head, pair.last).get
                })

                Some(length)
            }


            /**
             * Returns the shortest path between the two given vertices
             * or None if no path exists
             */
            def shortestPathBetween(source:T, destination:T): Option[Seq[Edge[T]]] = {
                import scala.collection.mutable.Set
                // if (source == destination) return Some(Seq[Edge[T]](new Edge[T](source, destination, 0)))
                if ((source != null || destination != null) || source != destination) {
                    // create a mutable map of vertices and their distances from the source
                    val distances = Map[T, Long]()

                    var closest: Map[T, Long] = Map[T, Long]()

                    // create a mutable map of vertices and their previous vertices
                    val previous = Map[T, T]()

                    // create a mutable list of vertices that have been visited
                    val visited = Set[T]()
                    
                    var closestVertex: T = 0.asInstanceOf[T]

                    var notAPath: Boolean = false


                    // push the source vertex into distance map with 0
                    distances += (source -> 0L)

                    // push the source vertex into the previous map with placeholder
                    previous += (source -> destination)

                    // while visited size is less than the number of vertices
                    while (visited.size < vertices.size && !notAPath) {

                        closest = distances.filter(distance => !visited.contains(distance._1))
                        
                        // if there are no more vertices to visit, then there is no path
                        if (closest.isEmpty) {
                            
                            notAPath = true

                        } else {
                            // get the closest vertex
                            closestVertex = closest.minBy(_._2)._1

                            // add the closest vertex to the visited set
                            visited += closestVertex

                            // for other in graph.adjacent(current) and other not in visited do
                            for (other <- getAdjacent(closestVertex) if !visited.contains(other)) {

                                // newDist = graph.edgeW eight(current, other) + dist(current)
                                var newDist = getEdgeWeight(closestVertex, other).get + distances(closestVertex)

                                if (newDist < distances.getOrElse(other, Long.MaxValue) || !distances.contains(other)) {

                                    // dist.push(other, newDist)
                                    distances += (other -> newDist)

                                    // parent.push(other, current)
                                    previous += (other -> closestVertex)
                                }
                            }
                        }
                    }

                    // return the shortest path between the source and destination
                    var path = IndexedSeq[T]()
                    var current = destination

                    // while current is not the source
                    while (current != source) {

                        // add current to the beginning of the path
                        path = current +: path

                        // if current is not in the previous map, then there is no path
                        if (!previous.contains(current)) {
                            return None
                        } else {
                            // set current to the previous vertex of current
                            current = previous(current)
                        }
                    }

                    // add the source to the beginning of the path
                    path = source +: path

                    // return edge list of the path
                    Some(path.sliding(2).map(pair => new Edge[T](pair(0), pair(1), getEdgeWeight(pair(0), pair(1)).get)).toSeq)

                } else {
                    None
                }

            }


            def greedyTSP():Seq[Edge[T]] = {
                greedyTSP(vertices.toSeq ++ Seq(vertices.toSeq.head))
            }


            def greedyTSP(initialTour:Seq[T]): Seq[Edge[T]] = {  
                
                var tour = initialTour
                
                var improving = true;

                // while there is improvement in the tour length
                while (improving) {

                    improving = false;

                    // for i = 0 until len(tour) - 1 do
                    for (i <- 0 until tour.size - 1 if tour.size > 2) {

                        for (j <- i + 1 until tour.size) {
                            
                            
                            // prefix = tour[0 : i]
                            // mid = tour[i : k]
                            // end = tour[k : length(tour)]
                            val prefix = tour.slice(0, i)
                            val mid = tour.slice(i, j)
                            val end = tour.slice(j, tour.length)

                            // prefix + reverse(mid) + end
                            val newTour = prefix ++ mid.reverse ++ end
                            var bestDist = pathLength(tour).getOrElse(0L)

                            // dist = graph.pathLength(newTour)
                            var dist: Long = pathLength(newTour).getOrElse(0L) 

                            if (dist < bestDist) {
                                tour = newTour
                                bestDist = dist
                                improving = true
                            }

                        }
                    }
                }

                // connect the last edge to the start of the tour
                tour = tour :+ tour.head

                // return seq of edges
                // do not add negative weight edges to the tour
                
                for (i <- 0 until tour.size - 1 if getEdgeWeight(tour(i), tour(i + 1)).isDefined) yield {
                    
                    val edge = new Edge[T](tour(i), tour(i + 1), getEdgeWeight(tour(i), tour(i + 1)).getOrElse(0))
                    (edge)

                }
    

            }



            def dynamicTSP:Seq[Edge[T]] = {

                if (vertices.isEmpty || edges.isEmpty) Seq[Edge[T]]()

                // depot is the first vertex
                var depot: T = vertices.head

                // Map dist = a Map from a Set of vertices and a vertex to a distance (number), initially empty Map
                var dist = HashMap[(Set[T], T), Long]()

                // parent = a Map from a (Set of vertices and a vertex )to a vertex, initially empty
                var parent = HashMap[(Set[T], T), T]()

                // List ends = graph.vertices \ depot
                // Vector of ends of vertices without the depot
                var ends: Set[T] = vertices.tail.toSet
                
                // BASE CASE CLEARED!
                for (k <- ends) {
                    // dist({k}, k) = graph.edgeW eight(depot, k)
                    dist += ((Set(k), k) -> getEdgeWeight(depot, k).get)

                    // parent({k}, k) = depot
                    parent += ((Set(k), k) -> depot)
                }

                // RECURSIVE CASE
                for (subSize <- 2 to ends.size) {

                    for (hist <- ends.subsets(subSize)) {
                        for (k <- hist) {

                            // dist(hist, k) = minBy x∈hist\k = dist(hist \ k, x) + graph.edgeWeight(x, k)
                            var min = Long.MaxValue
                            var minVertex = (hist - k).head

                            for (x <- hist - k) {
                                val current = dist((hist - k, x)) + getEdgeWeight(x, k).get
                                if (current < min) {
                                    minVertex = x
                                    min = current
                                }
                            }

                            dist += ((hist, k) -> min)
                            parent += ((hist, k) -> minVertex)
                        }
                    } 
                }
                var opt: T = depot

                // opt = argmin dist(ends, x) + graph.edgeW eight(x, depot) x∈ends
                opt = ends.minBy(x => dist((ends, x)) + getEdgeWeight(x, depot).get)

                // start rewinding the tour
                var tour: mutable.ListBuffer[T] = mutable.ListBuffer(depot)

                // while opt != depot do
                while (opt != depot) {
                    val newOne = parent((ends, opt))
                    tour.append(opt)
                    ends -= opt
                    opt = newOne
                }

                // connect the last edge to the start of the tour
                tour.append(opt)

                // return seq of edges
                for (i <- 0 until tour.size - 1) yield {
                    val edge = new Edge[T](tour(i), tour(i + 1), getEdgeWeight(tour(i), tour(i + 1)).get)
                    (edge)
                }
            }


            /**
             * Returns a string representation of the graph
             *
             * @return a string representation of the graph
             */
            override def toString:String = {
                // print the whole graph
                val sb = new StringBuilder
                
                val vertices = getVertices
                val _edges = edges

                sb.append("Vertices: ")
                sb.append(vertices.mkString(", "))

                sb.append("\nEdges: ")
                sb.append(_edges.mkString(", "))

                sb.toString()
            }
        }
    }

    def main(args: Array[String])
    {
        // var nonTrivialGraph = Graph[String](false)
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph_80_approx736.csv")
        var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph5_271.csv")
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph_10_319.csv")
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/Example3.csv")


        var path = undirectedGraph.dynamicTSP

         // add the last edge to end of path
        var onlyVerticesFromPath = path.map(edge => edge.source) :+ path.head.source
        var length = undirectedGraph.pathLength(onlyVerticesFromPath)

        // check edgeExists in path
        var edgeExists = path.map(edge => undirectedGraph.edgeExists(edge.source, edge.destination))
        // println(edgeExists)

        // check in reverse path
        var reversePath = path.reverse

        // check edgeExists in reverse path
        var reverseEdgeExists = reversePath.map(edge => undirectedGraph.edgeExists(edge.source, edge.destination))
        // println(reverseEdgeExists)

    }
}
