import java.io.{File, IOException}
import java.util.Scanner
import scala.util.Try
import scala.collection.mutable.{Map, Set}
import scala.collection.mutable.Stack
import scala.util.{Random, Try}
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

object graph
{
    /*
    A trait for representing directed and undirected graphs
    */
    trait Graph[T]
    {
        def isDirected:Boolean

        def getVertices:Iterable[T]

        def edgeExists(source:T, destination:T):Boolean

        def getEdgeWeight(source:T, destination:T):Option[Int]

        @throws(classOf[IllegalArgumentException])
        def addVertex(vertex:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def addEdge(source:T, destination:T, weight:Int):Graph[T]

        def pathLength(path:Seq[T]):Option[Long]

        def branchBoundTSP:Seq[Edge[T]]

        def branchBoundTSP(heur:(Graph[T], Seq[T]) => Long):Seq[Edge[T]]

        def heur(graph: Graph[T], tour: Seq[T]): Long

        def greedyTSP(initialTour:Seq[T]): Seq[Edge[T]]

        def greedyTSP: Seq[Edge[T]]

        def geneticTSP(popSize:Int, inversionProb:Float, maxIters:Int):Seq[Edge[T]]

        def geneticTSP:Seq[Edge[T]]

        def geneticTSP(initPop:Seq[Seq[T]], inversionProb:Float, maxIters:Int):Seq[Edge[T]]


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
                val vertices: HashSet[String] = HashSet[String]()

                for (i <- 0 until numVertices)
                {
                    vertices.add(scanner.nextLine())
                }

                // read the number of edges
                val numEdges = scanner.nextLine().toInt

                // read the edges
                val edges: HashMap[(String,String),Int] = HashMap[(String,String),Int]()

                for (i <- 1 to numEdges)
                {
                    val edge = scanner.nextLine().split(",")
                    edges.put((edge(0), edge(1)), edge(2).toInt)
                }

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
            val vertices: HashSet[T],
            val edges: HashMap[(T,T), Int]
        ) extends Graph[T]
        {
            /**
             * Returns the vertices of the graph
             * @return the list of vertices in the graph
             */
            def getVertices:Iterable[T] = vertices


            /**
             * Returns true if an edge exists between the two given vertices
             * @return true if an edge exists between the two given vertices
             */
            def edgeExists(source:T, destination:T):Boolean = {
                
                if (source == destination)
                    return true
                
                if (isDirected)
                    return edges.contains((source, destination))
                else
                    return edges.contains((source, destination)) || edges.contains((destination, source))
            }


            /**
             * Returns the weight of the edge between the two given vertices
             * @return the weight of the edge between the two given vertices
             */
            def getEdgeWeight(source:T, destination:T):Option[Int] = {
                
                if (source == destination)
                    return Some(0)
                
                if (isDirected)
                    edges.get((source, destination))
                else
                    edges.get((source, destination)).orElse(edges.get((destination, source)))
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

            def heur(graph: Graph[T], tour: Seq[T]): Long = {
                val heuristic = graph.getVertices.size - tour.size
                heuristic.toLong
            }

            def branchBoundTSP(heur: (Graph[T], Seq[T]) => Long):Seq[Edge[T]] = {

                var depot = vertices.head
                // define empty stack (LIFO) of Seq[T]
                var stack = Stack[Vector[T]]()

                // define best path
                var bestPath: Vector[T] = Vector[T]()

                // push depot onto stack
                stack.push(Vector(depot))

                // define minCost as max infinity
                var minCost: Long = Long.MaxValue

                // while stack is not empty

                while (stack.nonEmpty) {

                    // current = stack.pop()
                    var current = stack.pop()

                    // if pathLength(current) + heur(graph, current) < minCost
                    if (pathLength(current).getOrElse(0L) + this.heur(this, current) < minCost) {

                        // if is complete tour of current
                        if (current.length == vertices.size + 1 && current.head == current.last) {
                            // bestPath = current
                            bestPath = current

                            // minCost = pathLength(current)
                            minCost = pathLength(current).getOrElse(0L)
                        
                        } else {

                            // if current.size() < graph.vertices.size()
                            if (current.length < vertices.size) {

                                // for vertex in graph and not in current
                                for (vertex <- vertices if !current.contains(vertex)) {

                                    // push current + vertex onto stack
                                    stack.push(current :+ vertex)
                                }
                            } else {
                                stack.push(current :+ depot)
                            }
                        }
                    }
                }

                // add start to end
                bestPath = bestPath :+ depot

                // return bestPath
                for (i <- 0 until bestPath.size - 1) yield {
                    val edge = new Edge[T](bestPath(i), bestPath(i + 1), getEdgeWeight(bestPath(i), bestPath(i + 1)).getOrElse(0))
                    (edge)
                }

            }

            def branchBoundTSP:Seq[Edge[T]] = {
                branchBoundTSP(heur)
            }

            def greedyTSP: Seq[Edge[T]] = {
                greedyTSP(vertices.toVector ++ Seq(vertices.toVector.head))
            }


            def greedyTSP(initialTour:Seq[T]): Seq[Edge[T]] = {  
                
                var tour = initialTour.toVector
                
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
                
                for (i <- 0 until tour.size - 1) yield {
                    
                    val edge = new Edge[T](tour(i), tour(i + 1), getEdgeWeight(tour(i), tour(i + 1)).getOrElse(0))
                    (edge)
                }
            }


            def geneticTSP(popSize:Int, inversionProb:Float, maxIters:Int):Seq[Edge[T]] = {

                // pop = a random collection of tours of popSize
                var pop: Vector[Vector[T]] = Vector[Vector[T]]()

                // for i = 0 to popSize - 1 do
                for (i <- 0 until popSize) {

                    // pop.add(randomTour(graph))
                    pop = pop :+ Random.shuffle(vertices.toVector)
                }
    
                geneticTSP(pop, inversionProb, maxIters)
            }

            def geneticTSP:Seq[Edge[T]] = {
                geneticTSP(100, 0.2f, 1000)
            }


            def geneticTSP(initPop:Seq[Seq[T]], inversionProb:Float, maxIters:Int):Seq[Edge[T]] = {

                var pop: Vector[Vector[T]] = initPop.map(i => i.toVector).toVector

                var startCity: T = vertices.toVector.head
                var endCity: T = vertices.toVector.head

                // for a fixed number of iterations do
                for (i <- 1 to maxIters) {

                    // for tour in pop do
                    for (tour <- pop) {

                        // newTour = copy(tour)
                        var newTour: Vector[T] = tour
                        var otherTour: Vector[T] = tour

                        // startCity = randomCity(newTour)
                        startCity = Random.shuffle(newTour).head

                        var repeat = true

                        while (repeat) {

                            if (Random.nextFloat() <= inversionProb) {
                                // endCity = randomCity(newTour \ startCity)
                                // temporarily remove the startCity from the tour
                                if (newTour.size > 2) {

                                    while (endCity == startCity) {
                                        endCity = Random.shuffle(newTour).head
                                    }
                                }
                            }
                            else {
                                while (otherTour == tour) {
                                    // otherTour = randomTour(pop \ tour)
                                    // temporarily remove the tour from the population (pop)
                                    otherTour = pop(Random.nextInt(pop.size))
                                }

                                // endCity = city next to startCity in otherTour 
                                // while endCity is startCity or endCity is not next to startCity in otherTour do                               
                                while (endCity == startCity) {     
                                    endCity = otherTour(Random.nextInt(otherTour.size))
                                }
                                
                            }
    
                            if (newTour.indexOf(startCity) + 1 == newTour.indexOf(endCity)
                                || newTour.indexOf(startCity) - 1 == newTour.indexOf(endCity)) {
                                repeat = false
                            }
                            else {
                                // reverse a subsection of newTour between startCity and endCity
                                // prefix is the part of newTour before startCity inclusive
                                val prefix = newTour.slice(0, newTour.indexOf(startCity))
                                val mid = newTour.slice(newTour.indexOf(startCity), newTour.indexOf(endCity) + 1)
                                val end = newTour.slice(newTour.indexOf(endCity) + 1, newTour.size)

                                // prefix + reverse(mid) + end
                                newTour = prefix ++ mid.reverse ++ end

                                startCity = endCity 
                            }
                        }

                        //if tourLength(newTour) < tourLength(tour) then replace tour with newTour in pop
                        if (pathLength(newTour.map(vertex => vertex)).get < pathLength(tour.map(vertex => vertex)).get) {
                            
                            // replace tour with newTour in pop
                            pop = pop.updated(pop.indexOf(tour), newTour)
                        }
                    }
                }

                // return the shortest tour in pop
                var bestTour = pop.head

                bestTour = pop.minBy(tour => pathLength(tour.map(vertex => vertex)).get) 

                // append the start city to the end of the tour
                bestTour = bestTour :+ bestTour.head

                bestTour.sliding(2).map(edge => new Edge[T](edge(0), edge(1), getEdgeWeight(edge(0), edge(1)).getOrElse(0))).toVector
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
                val _edges = edges.toVector

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
        var nonTrivialGraph = Graph[String](false)
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph_80_approx736.csv")
        // var undirectedGraph = Graph.fromCSVFile(false , "src/main/graph5_271.csv")
        print("Graph (2Opt)," + "Tour Length," + "Time")
        for (i <- 3 until 41) {
            val undirectedGraph = Graph.fromCSVFile(false, "src/main/data/graph_" + i + ".csv")
            val start = System.currentTimeMillis()
            val tour = undirectedGraph.greedyTSP
            val end = System.currentTimeMillis()

            val onlyVerticesFromTour = tour.toVector.map(edge => edge.source) :+ tour.toVector.head.source
            // print("\n" + i + ",")
            print("\n" + undirectedGraph.pathLength(onlyVerticesFromTour.toVector).getOrElse(0L) + ",")

            val time = (end - start) / 1000.0
            print(time)
        }
    }
}
