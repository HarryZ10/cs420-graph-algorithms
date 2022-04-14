import java.io.{File, IOException}
import java.util.Scanner
import scala.util.Try
import scala.collection.mutable.{Map, Set}
import scala.collection.mutable.Stack
import scala.util.{Random, Try}

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

        def branchBoundTSP:Seq[Edge[T]]

        def branchBoundTSP(heur:(Graph[T], Seq[T]) => Long):Seq[Edge[T]]

        def heur(graph: Graph[T], tour: Seq[T]): Long

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
                var edgeSeq: Seq[Edge[T]] =
                    edges.map(edge => new Edge(edge._1, edge._2, edge._3))

                if (!isDirected) {
                    // for each edge, delete duplicate weights
                    edgeSeq = edgeSeq.map(edge =>
                    {
                        val newEdge = new Edge(edge.destination, edge.source, edge.weight)
                        newEdge
                    })

                    // sort the edges
                    edgeSeq = edgeSeq.sortWith((edge2, edge1) =>
                    {
                        if (edge2.weight > edge1.weight) false
                        else if (edge2.weight < edge1.weight) true
                        else false

                    })

                    // remove duplicate edges (same source and destination) but
                    // don't delete different edges with the same weight
                    edgeSeq = edgeSeq.foldLeft(Seq[Edge[T]]())((acc, edge) =>
                    {
                        if (acc.isEmpty ||((acc.last.weight != edge.weight) && (acc.head.source != edge.source && acc.head.destination != edge.destination)))
                            acc :+ edge
                        else
                            acc
                    })

                    // add back the edges with the same weight but different source and destination
                    for (i <- 0 until edges.size)
                    {
                        // Conditions
                        // add missing edges with the same weight
                        // the edges cannot be a reverse of an existing edge
                        // the edges cannot be a duplicate of an existing edge

                        // if the edge is not a reverse of an existing edge
                        if (!edgeSeq.exists(edge => edge.source == edges(i)._2 && edge.destination == edges(i)._1 && edge.weight == edges(i)._3))
                        {
                            // if the edge is not a duplicate of an existing edge
                            if (!edgeSeq.exists(edge => edge.source == edges(i)._1 && edge.destination == edges(i)._2 && edge.weight == edges(i)._3))
                            {
                                edgeSeq = edgeSeq :+ new Edge(edges(i)._1, edges(i)._2, edges(i)._3)
                            }
                        }
                    }
                    
                    
                }

                edgeSeq
            }

            def getEdge(source:T, destination:T):Option[Edge[T]] = {

                // get the edge from the edges list
                val edge = edges.find(e => e._1 == source && e._2 == destination)
                // if edge exists, return it
                if (edge.isDefined)
                    Some(new Edge(edge.get._1, edge.get._2, edge.get._3))
                else
                    None
            }


            /**
             * Returns true if an edge exists between the two given vertices
             * @return true if an edge exists between the two given vertices
             */
            def edgeExists(source:T, destination:T):Boolean = {
                
                if (isDirected)
                {
                    edges.exists(e => e._1 == source && e._2 == destination)
                }
                else
                {
                    edges.exists(e => e._1 == source && e._2 == destination) ||
                    edges.exists(e => e._1 == destination && e._2 == source)
                }
            }


            /**
             * Returns the weight of the edge between the two given vertices
             * @return the weight of the edge between the two given vertices
             */
            def getEdgeWeight(source:T, destination:T):Option[Int] = {
                
                // count each edge once even if it's undirected
                var edge = edges.find(e => e._1 == source && e._2 == destination)

                if (!isDirected) {
                    // if the edge is not found, check if it's a reverse edge
                    if (edge.isEmpty)
                        edge = edges.find(e => e._1 == destination && e._2 == source)
                }

                // if edge exists, return it
                if (edge.isDefined)
                    Some(edge.get._3)
                else {
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
                        if (isDirected)
                        {
                            new GraphImpl(isDirected, vertices, edges :+ (source, destination, weight))
                        }
                        else
                        {
                            new GraphImpl(isDirected, vertices, edges :+ (source, destination, weight) :+ (destination, source, weight))
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
                        val newEdges = edges.filterNot(edge => edge._1 == source && edge._2 == destination)
                        new GraphImpl(isDirected, vertices, newEdges)
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
                var dist = Map[T, Int]()
                // immutable map parent 
                var parent: Map[T, T] = Map[T, T]()

                var visited = Set[T]()

                var closest: Map[T, Int] = Map[T, Int]()
                var current = 0.asInstanceOf[T]
                var start = 0.asInstanceOf[T]
                var complete: Boolean = true

                if (vertices.isEmpty || edges.isEmpty) 
                {
                    None
                }
                else
                {
                    start = vertices.head

                    if (!isDirected) {

                        // Initialize parent and dist with vertices adjacent to start
                        var parent = vertices.map(v => (v, start)).toMap
                        var dist = edges.filter(e => e._1 == start).map(e => (e._2, e._3)).toMap

                        // Initialize tree with vertices
                        for (v <- vertices) {
                            tree = tree.addVertex(v)
                        }

                        // // Initialize visited with start
                        visited += start

                        // while visited is not equal to vertices
                        while (visited.size < vertices.length && complete)
                        {

                            // find closest vertex
                            var closest = dist.filter(d => !visited.contains(d._1))

                            if (closest.isEmpty)
                            {
                                complete = false
                            }
                            else 
                            {
                                current = closest.minBy(_._2)._1

                                visited += current

                                tree = tree.addEdge(parent(current), current, dist(current)) 

                                tree = tree.removeEdge(current, parent(current))   

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

                    if (!complete && visited.size < tree.getVertices.size - 1)
                    {
                        None
                    }
                    else
                    {
                        Some(tree)
                    }
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

                edges.filter(e => (e._1 == source)).map(e => e._2)
            }


            /*
             * Returns length of path given a list of vertices
             * or None if no path exists
             */
            def pathLength(path: Seq[T]): Option[Long] = {

                var length = 0L
                var notAPath: Boolean = false;
                var returnLength: Option[Long] = Option.empty[Long]

                path.toVector.sliding(2).foreach(pair => {
                    if (getEdgeWeight(pair.head, pair.last).isDefined) {
                        length += getEdgeWeight(pair.head, pair.last).get

                    } else notAPath = true
                })

                if (!notAPath) returnLength = Try(length).toOption

                returnLength
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

                if (vertices.isEmpty || edges.isEmpty) {
                    Seq[Edge[T]]()
                }

                // depot is the first vertex
                var depot: T = vertices.head

                // Map dist = a Map from a Set of vertices and a vertex to a distance (number), initially empty Map
                var dist = Map[Set[T], Map[T, Int]]()

                // parent = a Map from a Set of vertices and a vertex to a vertex, initially empty
                var parent = Map[Set[T], Map[T, T]]()

                // List ends = graph.vertices \ depot
                // Vector of ends of vertices without the depot
                var ends: Set[T] = Set[T]()

                // for each vertex in ends do
                for (vertex <- vertices.tail) {
                    // ends.push(vertex)
                    ends += vertex
                }

                // remove the depot from ends
                ends -= depot

                // BASE CASE CLEARED!
                for (k <- ends) {

                    // dist ({k},k) = edgeWeight(depot, k)
                    // put edgeWeight of depot and k in dist at key k and value
                    dist(Set(k)) = Map(k -> getEdgeWeight(depot, k).getOrElse(0))

                    // parent ({k},k) = depot
                    // put depot in parent at key k and value
                    parent(Set(k)) = Map(k -> depot)
                }

                // RECURSIVE CASE
                for (subSize <- 2 to ends.size) {

                    //for hist in all subsets of ends of size = subSize
                    for (hist <- ends.subsets(subSize)) {

                        // for k in hist
                        for (k <- hist) {

                            var saveX = 0.asInstanceOf[T]
                            
                            // x∈hist\k for all x∈hist
                            for (x <- hist if x != k) {

                                var newHistTemp = hist.filter(v => v != k)
                                // dist(hist, k) = min(dist(hist \ k, x) + edgeWeight(x, k))
                                // if x is in dist at key newHistTemp
                                
                                if (dist.contains(newHistTemp))
                                {
                                    dist(hist) = Map(k -> (dist(newHistTemp).getOrElse(x, 0) + getEdgeWeight(x, k).getOrElse(0)))
                                    saveX = x
                                }
                            }

                            // parent(hist, k) = x
                            parent(hist) = Map(k -> saveX)
                        }
                    }
                }

                // optimum is the argmin of x in ends of dist(ends, x) + edgeWeight(x, depot)
                var opt: T = 0.asInstanceOf[T]

                // for each x in ends
                for (x <- ends if dist(ends).getOrElse(x, 0) + getEdgeWeight(x, depot).getOrElse(0) < dist(ends).getOrElse(opt, 0) + getEdgeWeight(opt, depot).getOrElse(0)) {

                    // if dist(ends, x) + edgeWeight(x, depot) < dist(ends, optimum)
                    if (dist(ends).getOrElse(x, 0) + getEdgeWeight(x, depot).getOrElse(0) < dist(ends).getOrElse(opt, 0)) {

                        // optimum = x
                        opt = x
                    }
                }

                // path = [depot]
                var path = Seq[T](depot)

                // while optimum is not depot
                while (opt != depot) {

                    // path.push(optimum)
                    path = opt +: path

                    // optimum = parent(path, optimum)
                    opt = parent(Set() ++ path).getOrElse(opt, 0.asInstanceOf[T])
                }

                path = depot +: path

                // return seq of edges
                // do not add negative weight edges to the tour
                for (i <- 0 until path.size - 1 if getEdgeWeight(path(i), path(i + 1)).isDefined) yield {
                    val edge = new Edge[T](path(i), path(i + 1), getEdgeWeight(path(i), path(i + 1)).getOrElse(0))
                    (edge)
                }
            }


            def heur(graph: Graph[T], tour: Seq[T]): Long = {
                val heuristic = graph.getVertices.size - tour.size
                heuristic.toLong
            }

            def branchBoundTSP(heur: (Graph[T], Seq[T]) => Long):Seq[Edge[T]] = {

                var depot = vertices.head
                // define empty stack (LIFO) of Seq[T]
                var stack = Stack[Seq[T]]()

                // define best path
                var bestPath: Seq[T] = Seq[T]()

                // push depot onto stack
                stack.push(Seq(depot))

                // define minCost as max infinity
                var minCost: Long = Long.MaxValue

                // while stack is not empty

                while (stack.nonEmpty) {

                    // current = stack.pop()
                    var current = stack.pop()

                    // if pathLength(current) + heur(graph, current) < minCost
                    if (pathLength(current).getOrElse(0L) + this.heur(this, current) < minCost) {

                        // if is complete tour of current
                        if (current.length == vertices.length + 1) {
                            // bestPath = current
                            bestPath = current

                            // minCost = pathLength(current)
                            minCost = pathLength(current).getOrElse(0L)
                        
                        } else {

                            // if current.size() < graph.vertices.size()
                            if (current.length < vertices.length) {

                                // for vertex in graph and not in current
                                for (vertex <- vertices if !current.contains(vertex)) {

                                    // push current + vertex onto stack
                                    stack.push(current :+ vertex)
                                }
                            }
                        }
                    }
                }

                // add start to end
                bestPath = bestPath :+ depot

                // return bestPath
                for (i <- 0 until bestPath.size - 1 if getEdgeWeight(bestPath(i), bestPath(i + 1)).isDefined) yield {
                    val edge = new Edge[T](bestPath(i), bestPath(i + 1), getEdgeWeight(bestPath(i), bestPath(i + 1)).getOrElse(0))
                    (edge)
                }

            }

            def branchBoundTSP:Seq[Edge[T]] = {
                branchBoundTSP(heur)
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

    def main(args: Array[String])
    {
        var nonTrivialGraph = Graph[String](false)
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph_80_approx736.csv")
        // var undirectedGraph = Graph.fromCSVFile(false , "src/main/graph5_271.csv")
        var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph_10_319.csv")

        // reverse the middle of the path
        // path = path.slice(0, path.size / 2) ++ path.slice(path.size / 2 + 1, path.size).reverse

        var branchbound = undirectedGraph.branchBoundTSP
        // var branchbound = undirectedGraph.branchBoundTSP(undirectedGraph.heur(_,_))
        // var length = undirectedGraph.pathLength(vertex => branchbound.map(_.sourc).contains(vertex))

        println("Branch and Bound: " + branchbound)
        println("Length: " + length)
        

    }
}
