import java.io.{File, IOException}
import java.util.Scanner
import scala.collection.mutable.{Map, Set}
import scala.util.{Random, Try}
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import java.io.PrintWriter
import java.io.FileOutputStream

object graph
{
    /*
    A trait for representing directed and undirected graphs
    */
    trait Graph[T]
    {
        def isDirected:Boolean

        def getVertices:Iterable[T]

        def getEdgeWeight(source:T, destination:T):Option[Int]

        @throws(classOf[IllegalArgumentException])
        def addVertex(vertex:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def addEdge(source:T, destination:T, weight:Int):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def removeEdge(source:T, destination:T):Graph[T]

        def pathLength(path:Seq[T]):Option[Long]

        def geneticTSP(popSize:Int, inversionProb:Float, maxIters:Int):Seq[Edge[T]]

        def geneticTSP:Seq[Edge[T]]

        def geneticTSP(initPop:Seq[Seq[T]], inversionProb:Float, maxIters:Int):Seq[Edge[T]]

        def toString:String
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
            import scala.collection.mutable.HashSet
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

                // read the edges as HashMap[(source, destination), weight]
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
        private class GraphImpl[T]
            (val isDirected:Boolean, val vertices: HashSet[T], val edges: HashMap[(T, T), Int]) extends Graph[T]
        {
            /**
             * Returns the vertices of the graph
             * @return the list of vertices in the graph
             */
            def getVertices:Iterable[T] = vertices

            /**
             * Returns the weight of the edge between the two given vertices
             * @return the weight of the edge between the two given vertices
             */
            def getEdgeWeight(source:T, destination:T):Option[Int] = {
                
                // count each edge once even if it's undirected
                val edge = (source, destination)
                if (edges.contains(edge)) Some(edges(edge))
                else if (edges.contains((destination, source))) Some(edges((destination, source)))
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
                    if (edges.contains((source, destination)) || edges.contains((destination, source)))
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
                            new GraphImpl(isDirected, vertices, edges + ((source, destination) -> weight))
                        }
                        else
                        {
                            new GraphImpl(isDirected, vertices, edges + ((source, destination) -> weight) + ((destination, source) -> weight))
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
                    // if edge exists, remove it
                    if (edges.contains((source, destination)))
                    {
                        new GraphImpl(isDirected, vertices, edges - ((source, destination)))
                    }
                    else if (edges.contains((destination, source)))
                    {
                        new GraphImpl(isDirected, vertices, edges - ((destination, source)))
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

            /*
             * Returns length of path given a list of vertices
             * or None if no path exists
             */
            def pathLength(path: Seq[T]): Option[Long] = {
                var length = 0L
                path.toVector.sliding(2).foreach(pair => {
                    length += getEdgeWeight(pair.head, pair.last).getOrElse(0)
                })

                Some(length)
            }


            def geneticTSP(popSize:Int, inversionProb:Float, maxIters:Int):Seq[Edge[T]] = {

                // pop = a random collection of tours of popSize
                var pop: Set[Set[T]] = Set[Set[T]]()

                for (i <- 0 until popSize) {
                    var tour: Set[T] = Set[T]()
                    for (j <- 0 until vertices.size) {
                        tour += vertices.toVector(Random.nextInt(vertices.size))
                    }
                    pop += tour
                }

                var seq: Seq[Seq[T]] = (pop.toSeq.map(tour => tour.toSeq))

                geneticTSP(seq, inversionProb, maxIters)
            }

            def geneticTSP:Seq[Edge[T]] = {
                geneticTSP(100, 0.2f, 1000)
            }


            def geneticTSP(initPop:Seq[Seq[T]], inversionProb:Float, maxIters:Int):Seq[Edge[T]] = {

                var pop: Vector[Vector[T]] = initPop.map(i => i.toVector).toVector

                var startCity: T = vertices.toVector.head
                var endCity: T = vertices.toVector.head
                var inversions: Int = 0
                var iterationCount: Int = 0

                // for a fixed number of iterations do
                for (i <- 1 to maxIters) {

                    // for tour in pop do
                    for (tour <- pop if tour.size > 2) {

                        // newTour = copy(tour)
                        var newTour: Vector[T] = tour
                        var otherTour: Vector[T] = tour

                        // startCity = randomCity(newTour)
                        startCity = Random.shuffle(newTour).head

                        var repeat = true

                        while (repeat) {

                            iterationCount += 1

                            if (Random.nextFloat() <= inversionProb) {
                                inversions += 1
                                // endCity = randomCity(newTour \ startCity)
                                // temporarily remove the startCity from the tour
                                while (endCity == startCity) {
                                    endCity = Random.shuffle(newTour.tail).head
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
                                    endCity = Random.shuffle(otherTour.tail).head
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

                //print inversions
                println("Inversions: " + inversions)
                println("Iterations: " + iterationCount)

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
                val my_edges = edges.toVector

                sb.append("Vertices: ")
                sb.append(vertices.mkString(", "))

                sb.append("\nEdges: ")
                sb.append(my_edges.mkString(", "))

                sb.toString()
            }
        }
    }

    def main(args: Array[String])
    {
        // var nonTrivialGraph = Graph[String](false)
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph_80_approx736.csv")
        // var undirectedGraph = Graph.fromCSVFile(false, "src/main/graph5_271.csv")
        var name = "src/main/Example.csv"
        var undirectedGraph = Graph.fromCSVFile (false, name)
        var path = undirectedGraph.geneticTSP(100, 0.2f, 90000)

        // add the last edge to end of path
        var onlyVerticesFromPath = path.map(edge => edge.source) :+ path.head.source
        var length = undirectedGraph.pathLength(onlyVerticesFromPath)

        println(path)
        println(length)

        // write the length and name
        var writer = new PrintWriter(new FileOutputStream(
            new File("src/main/scala/output.csv"), 
            true /* append = true */));

        writer.append("\n")

        // push the length to the file
        writer.append(name + ",").append(length.toString + ",")

        // get weights from path
        var weights = path.map(edge => edge.weight)

        // push the weights to the file
        writer.append(weights.mkString(",") + "\n")

        // close the file
        writer.close()
    }
}
