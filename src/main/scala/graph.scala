import java.io.IOException

object graph
{
    /*
    A trait for representing directed and undirected graphs
    */
    trait Graph[T]
    {
        def isDirected:Boolean

        def getVertices:Iterable[T]

        def getEdges:Iterable[(T,T,Int)]

        def edgeExists(source:T, destination:T):Boolean

        def getEdgeWeight(source:T, destination:T):Option[Int]

        @throws(classOf[IllegalArgumentException])
        def addVertex(vertex:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def removeVertex(vertex:T):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def addEdge(source:T, destination:T, weight:Int):Graph[T]

        @throws(classOf[IllegalArgumentException])
        def removeEdge(source:T, destination:T):Graph[T]

        override def toString:String
    }

    /**
    Serves as a factory function for producing new empty Graphs
    */
    object Graph
    {
        /*
        Creates and returns a new empty Graph - acts as a constructor
        */
        def apply[T](isDirected: Boolean): Graph[T] =
        {
            val vertices: IndexedSeq[T] = IndexedSeq[T]()
            val edges: Seq[(T,T,Int)] = Seq[(T,T,Int)]()
            
            new GraphImpl(isDirected, vertices, edges)
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
            def getEdges:Iterable[(T, T, Int)] = edges


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
                    // remove all edges that contain the vertex
                    val newEdges = edges.filter(edge => edge._1 != vertex && edge._2 != vertex)

                    // remove the vertex
                    val newVertices = vertices.filter(v => v != vertex)

                    // create a new graph
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
                        val newEdges = edges.filter(edge => !(edge._1 == source && edge._2 == destination))
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
        //create an empty graph
        var undirectedGraph = Graph[String](false)

        //add some vertices of type String
        undirectedGraph = undirectedGraph.addVertex("A").addVertex("B").addVertex("C").addVertex("D")
        // println(undirectedGraph.getVertices.toString)

        //add some edges of type String
        undirectedGraph = undirectedGraph.addEdge("A", "B", 1).addEdge("A", "C", 2).addEdge("B", "C", 3).addEdge("C", "D", 4)

        //print the graph
        println(undirectedGraph)

        var directedGraph = Graph[String](true)

        //add some vertices of type String
        directedGraph = directedGraph.addVertex("A").addVertex("B").addVertex("C").addVertex("D")

        //add some edges of type String
        directedGraph = directedGraph.addEdge("A", "B", 1).addEdge("B", "A", 2).addEdge("A", "C", 3).addEdge("C", "A", 4)
        println(directedGraph)
       
        // remove a vertex
        directedGraph = directedGraph.removeVertex("B")

        // print the graph
        println(directedGraph)


        // add a vertex where the vertex already exists
        try
        {
            directedGraph = directedGraph.addVertex("A")
        }
        catch
        {
            case e:IllegalArgumentException => println("Vertex already exists")
        }

        // remove a vertex where the vertex does not exist
        try
        {
            directedGraph = directedGraph.removeVertex("H")
        }
        catch
        {
            case e:IllegalArgumentException => println("Vertex does not exist")
        }

        // adding a loop
        try
        {
            directedGraph = directedGraph.addEdge("A", "A", 1)
        }
        catch
        {
            case e:IllegalArgumentException => println("Loop not allowed")
        }

        // remove an edge where the edge does not exist
        try
        {
            directedGraph = directedGraph.removeEdge("A", "D")
        }
        catch
        {
            case e:IllegalArgumentException => println("Edge does not exist")
        }

        
        // add an edge where the edge already exists
        try
        {
            directedGraph = directedGraph.addEdge("A", "B", 1)
        }
        catch
        {
            case e:IllegalArgumentException => println("Edge already exists")
        }

        directedGraph = directedGraph.addVertex("B")

        directedGraph = directedGraph.addEdge("B", "A", 2)
        directedGraph = directedGraph.addEdge("A", "B", 30)

        // get edge weight
        println(directedGraph.getEdgeWeight("A", "B").get)
        println(directedGraph.getEdgeWeight("B", "A").get)

        // get vertices
        println(directedGraph.getVertices.toString)

        // get edges
        println(directedGraph.getEdges.toString)
    }
}
