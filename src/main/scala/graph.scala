import java.io.IOException
import java.io.File
import java.util.Scanner

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

                new GraphImpl(isDirected, vertices, edges)
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
                // create new seq from edges using map
                var edgeSeq: Iterable[Edge[T]] =
                    edges.map(edge => new Edge(edge._1, edge._2, edge._3))

                edgeSeq
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
                
                // count each edge once even if it's undirected
                val edge = edges.find(e => e._1 == source && e._2 == destination)

                // if edge exists, return it
                if (edge != None)
                    Some(edge.get._3)
                else
                    None
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

                        val newEdges = edges :+ (source, destination, weight)
                        new GraphImpl(isDirected, vertices, newEdges)
                        
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
                var tree = Graph[T](false)

                if (vertices.isEmpty) None
                else
                {
                    // if directed, return None
                    if (!isDirected) {

                        var dist = Map[T, Int]()
                        var parent = Map[T, T]()
                        var visited = Set[T]()
                        var closest: Map[T, Int] = Map[T, Int]()
                        var current = 0.asInstanceOf[T]
                        val start = vertices.head
                        var notAGraph: Boolean = false

                        // Initialize parent and dist with vertices adjacent to start
                        for (vertex <- vertices) 
                        {
                            if (edgeExists(start, vertex))
                            {
                                dist += (vertex -> getEdgeWeight(start, vertex).get)
                                parent += (vertex -> start)
                            }

                            tree = tree.addVertex(vertex)
                        }

                        // while visited is not equal to vertices
                        while (visited.size < vertices.length && !notAGraph)
                        {
                            closest = dist.filter(v => !visited.contains(v._1))
                            if (closest.isEmpty) notAGraph = true
                            else 
                            {
                                current = closest.minBy(_._2)._1
                                visited += current
                                tree = tree.addEdge(current, parent(current), dist(current))
                                for (other <- getAdjacent(current) if !visited.contains(other)) {
                                    
                                    var newDist = getEdgeWeight(current, other).getOrElse(Int.MaxValue)

                                    if (newDist < dist.getOrElse(other, Int.MaxValue) || !dist.contains(other)) {
                                        dist += (other -> newDist)
                                        parent += (other -> current)
                                    }
                                }      
                            }     
                        }
                    }
                }

                Some(tree)
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
                if ((source == null || destination == null) || source == destination) {
                    return None
                }

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

                    // get the previous vertex of current
                    try {
                        current = previous(current)
                    }
                    catch {
                        case e: NoSuchElementException => return None
                    }
                }

                // add the source to the beginning of the path
                path = source +: path

                // return edge list of the path
                Some(path.sliding(2).map(pair => new Edge[T](pair(0), pair(1), getEdgeWeight(pair(0), pair(1)).get)).toIndexedSeq)
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
        // Example.csv is a file with the following format:
        var undirectedGraph = Graph.fromCSVFile(false, "src/main/Example.csv")
        
        // print minimum spanning tree
        println("Minimum Spanning Tree:")
        println(undirectedGraph.minimumSpanningTree)

    }
}
