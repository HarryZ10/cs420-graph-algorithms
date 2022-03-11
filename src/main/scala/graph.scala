import java.io.IOException
import java.io.File
import java.util.Scanner

import scala.util.Random
import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.collection.mutable.Set

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

        def minimumSpanningTree:Option[Graph[T]]

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
            val vertices: IndexedSeq[T] = IndexedSeq[T]()
            val edges: Seq[(T,T,Int)] = Seq[(T,T,Int)]()
            
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
                val vertices = (1 to numVertices).map(i => scanner.nextLine())

                // read the number of edges
                val numEdges = scanner.nextLine().toInt

                // read the edges
                val edges = (1 to numEdges).map(i =>
                {
                    val edge = scanner.nextLine().split(",")
                    (edge(0), edge(1), edge(2).toInt)
                })

                scanner.close()

                if (isDirected)
                    new GraphImpl(isDirected, vertices, edges)
                else
                    new GraphImpl(isDirected, vertices, edges ++ edges.map(e => (e._2, e._1, e._3)))
            }
            catch {
                case e:Exception => throw new IOException("Error reading file " + fileName)
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
            val vertices: IndexedSeq[T],
            val edges: Seq[(T,T,Int)]
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
                if (isDirected)
                {
                    edges.map(edge => new Edge(edge._1, edge._2, edge._3))
                }
                else
                {
                    edges.map(edge => new Edge(edge._1, edge._2, edge._3)) ++
                    edges.map(edge => new Edge(edge._2, edge._1, edge._3))
                }
            }


            def getEdge(source:T, destination:T):Option[Edge[T]] = {

                // get the edge from the edges list
                val edge = edges.find(e => e._1 == source && e._2 == destination)

                // if edge exists, return it
                if (edge != None)
                    Some(new Edge(edge.get._1, edge.get._2, edge.get._3))
                else
                    None
            }


            /**
             * Returns true if an edge exists between the two given vertices
             * @return true if an edge exists between the two given vertices
             */
            def edgeExists(source:T, destination:T):Boolean = {
                edges.exists(edge => edge._1 == source && edge._2 == destination)
            }


            /**
             * Returns the weight of the edge between the two given vertices
             * @return the weight of the edge between the two given vertices
             */
            def getEdgeWeight(source:T, destination:T):Option[Int] = {
                
                if (edgeExists(source, destination))
                {
                    Some(edges.filter(edge => edge._1 == source && edge._2 == destination).head._3)
                }
                else
                {
                    None
                }
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
                    new GraphImpl(isDirected, vertices :+ vertex, edges)
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
                    // if the vertex is in any edges, remove them
                    val newEdges = edges.filter(edge => edge._1 != vertex && edge._2 != vertex)

                    // remove the vertex from the list of vertices
                    val newVertices = vertices.filter(v => v != vertex)

                    new GraphImpl(isDirected, newVertices, newEdges)
                }
                else
                {
                    throw new IllegalArgumentException("Vertex does not exist")
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
                        if (!isDirected)
                        {
                            val newEdges = edges :+ (source, destination, weight) :+ (destination, source, weight)
                            new GraphImpl(isDirected, vertices, newEdges)
                        } else {
                            val newEdges = edges :+ (source, destination, weight)
                            new GraphImpl(isDirected, vertices, newEdges)
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
                        if (!isDirected)
                        {
                            val newEdges = edges.filterNot(edge => edge._1 == source && edge._2 == destination)
                            val newEdges2 = newEdges.filterNot(edge => edge._1 == destination && edge._2 == source)
                            new GraphImpl(isDirected, vertices, newEdges2)
                        }
                        else
                        {
                            val newEdges = edges.filterNot(edge => edge._1 == source && edge._2 == destination)
                            new GraphImpl(isDirected, vertices, newEdges)
                        }
                    }
                    else
                    {
                        throw new IllegalArgumentException("Edge does not exist")
                    }
                }
                else
                {
                    throw new IllegalArgumentException("One or both vertices do not exist")
                }
            }

            def minimumSpanningTree:Option[Graph[T]] = {

                // if directed, return None
                if (!isDirected) {
                
                    // set of edges, initially empty
                    var out:Set[Edge[T]] = Set()

                    // a map from vertex to a set of vertices, initially empty
                    var tree:Map[T, Set[T]] = Map()

                    // a sorted (ascending) list of edges
                    var sortedEdges:Seq[(T, T, Int)] = edges.toList.sortWith((e1, e2) => e1._3 < e2._3)

                    // for each edge in the sorted list
                    for (edge <- sortedEdges if tree.size != vertices.size)
                    {

                        // if the source is not in the tree
                        if (!tree.contains(edge._1))
                        {
                            // add the source to the tree
                            tree = tree + (edge._1 -> Set(edge._1))

                            // if the destination is not in the tree
                            if (!tree.contains(edge._2))
                            {
                                // add the destination to the tree
                                tree = tree + (edge._2 -> Set(edge._2))
                            }
                        }

                        // if the destination is not in the tree
                        if (!tree.contains(edge._2))
                        {
                            // add the destination to the tree
                            tree = tree + (edge._2 -> Set(edge._2))
                        }

                        // if the source and destination are in the tree
                        if (tree.contains(edge._1) && tree.contains(edge._2))
                        {
                            // if the source and destination are not the same
                            if (edge._1 != edge._2)
                            {
                                // if the source and destination are not already connected
                                if (!tree(edge._1).contains(edge._2))
                                {
                                    // add the edge to the set of edges
                                    out = out + (new Edge(edge._1, edge._2, edge._3))

                                    // add the destination to the source's set of vertices
                                    tree = tree + (edge._1 -> (tree(edge._1) + edge._2))

                                    // add the source to the destination's set of vertices
                                    tree = tree + (edge._2 -> (tree(edge._2) + edge._1))
                                }
                            }
                        }
                    }

                    Some(new GraphImpl(isDirected, vertices, out.map(edge => (edge.source, edge.destination, edge.weight)).toSeq))
                } else {
                    None
                }
            }

            /**
             * Get adjacent vertices of the source vertex given
             */
            def getAdjacent(source:T):Iterable[T] = {
                if (vertices.contains(source)) edges.filter(edge => edge._1 == source).map(edge => edge._2)
                else throw new IllegalArgumentException("Vertex does not exist")
            }


            /*
             * Returns length of path given a list of vertices
             * or None if no path exists
             */
            def pathLength(path: Seq[T]): Option[Long] = {

                if (path.size < 2) return None
                
                var length = 0L
                var i = 0

                while (i < path.size - 1)
                {
                    // get source and destination vertices
                    val source = path(i)
                    val destination = path(i + 1)

                    // check if edge exists then add weight to length
                    // otherwise there is no path thus return None
                    if (edgeExists(source, destination))
                        length += getEdgeWeight(source, destination).get
                    else return None

                    // keep going until we reach the end of the path
                    i += 1
                }

                // return the length of the path
                Some(length)
            }


            /**
             * Returns the shortest path between the two given vertices
             * or None if no path exists
             */
            def shortestPathBetween(source:T, destination:T): Option[Seq[Edge[T]]] = {
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
                    while (visited.size < vertices.length && !notAPath) {

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

                var tour: Seq[T] = Seq[T]()
                tour = makeTour(vertices.toSeq)

                println("Tour: " + tour)
                
                var tourDist = pathLength(tour)

                // while tour dist is less than all the edge's weight
                while (tourDist.getOrElse(Long.MaxValue) < totalEdgeWeight) {

                    // for i = 0... length(tour) - 1
                    for (a <- 0 until tour.size - 1) {
                        for (b <- a + 1 until tour.size) {

                            // newTour gets 2Opt Swap of tour, i, j
                            var newTour = twoOptSwap(tour, a, b)
                            var dist = pathLength(newTour)

                            // if dist < bestDist
                            if (dist.getOrElse(0L) < tourDist.getOrElse(0L)) {
                                tour = newTour
                                tourDist = dist
                            }
                        }
                    }
                }

                // return tour
                tour.sliding(2).map(pair => new Edge[T](pair(0), pair(1), getEdgeWeight(pair(0), pair(1)).getOrElse(0))).toSeq
            }

            def makeTour(list: Seq[T]): Seq[T] = {

                // create a mutable list of vertices that have been visited
                var visited = Set[T]()
                
                // use a stack to keep track of vertices to visit
                var stack = Stack[T]()

                // get first vertex in list as a starting point
                var current = list(0)

                // push the first vertex into the stack
                stack.push(current)
                
                // mark the first vertex as visited
                visited += current

                // until the stack is empty
                while (!stack.isEmpty) {

                    // pop the top vertex from the stack to point to current
                    current = stack.pop()

                    // for each vertex in graph.adjacent(current)
                    for (other <- getAdjacent(current) if !visited.contains(other)) {

                        // if other is not in visited
                        // push other into stack
                        stack.push(other)

                        // mark other as visited
                        visited += other
                    }
                }

                // return the list of vertices that have been visited
                visited.toSeq
            }

            def twoOptSwap(tour: Seq[T], a: Int, b: Int): Seq[T] = {

                var prefix = tour.slice(0, a)
                var mid = tour.slice(a, b)
                var end = tour.slice(b, tour.size)

                // return prefix + reverse(mid) + end
                prefix ++ mid.reverse ++ end
            }
        
            // get total weight
            def totalEdgeWeight(): Long = {
                // if directed, return total weight
                if (isDirected) {
                    edges.map(edge => edge._3).sum
                } else {
                    // if undirected, return total weight / 2
                    edges.map(edge => edge._3).sum / 2
                }
            }

            def greedyTSP(initialTour:Seq[T]):Seq[Edge[T]] = {

                var tour: Seq[T] = initialTour
                var tourDist = this.pathLength(tour)

                // while tour dist is less than all the edge's weight
                while (tourDist.getOrElse(Long.MaxValue) < totalEdgeWeight) {
                    
                    // for i = 0... length(tour) - 1
                    for (i <- 0 until tour.size - 1) {
                        for (j <- i + 1 until tour.size) {

                            // newTour gets 2Opt Swap of tour, i, j
                            var newTour = twoOptSwap(tour, i, j)
                            var dist = this.pathLength(newTour)

                            // if dist < bestDist
                            if (dist.getOrElse(0L) < tourDist.getOrElse(0L)) {
                                tour = newTour
                                tourDist = dist
                            }
                        }
                    }
                }

                // return tour
                tour.sliding(2).map(pair => new Edge[T](pair(0), pair(1), getEdgeWeight(pair(0), pair(1)).getOrElse(0))).toSeq
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
                val edges = getEdges

                sb.append("Vertices: ")
                sb.append(vertices.mkString(", "))

                sb.append("\nEdges: ")
                sb.append(edges.mkString(", "))

                sb.toString()
            }
        }
    }

    def main(args:Array[String])
    {
        //create an empty graph from example.csv
        val graph = Graph.fromCSVFile(false, "src/main/Example.csv");
        println(graph.getEdges)
        println(graph.greedyTSP());
        // println(graph.greedyTSP(Seq("v1", "v2", "v3", "v4", "v5", "v6")))
        
    }
}
