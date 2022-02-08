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
		def apply[T](isDirected:Boolean):Graph[T] =
		{
			//TODO add new empty data structures here,
			//then pass them in as arguments to GraphImpl 
			new GraphImpl[T](isDirected) 
		}

		/*
		An private implmenentation of the Graph trait
		*/
		//TODO add in data structures to the constructor here


		
		//you could also consider a different implementation for directed vs undirected graphs...
		private class GraphImpl[T] (val isDirected:Boolean) extends Graph[T]
		{
			//TODO add in data structures here
			private var vertices:List[T] = List()
			private var edges:List[(T, T, Int)] = List()

			def getVertices:Iterable[T] = vertices

			def edgeExists(source:T, destination:T):Boolean = {
				edges.exists(e => e._1 == source && e._2 == destination)
			}
			
			def getEdgeWeight(source:T, destination:T):Option[Int] = {
				edges.find(e => e._1 == source && e._2 == destination).map(_._3)
			}

			def addVertex(vertex:T):Graph[T] = {
				if (vertices.contains(vertex))
					throw new IllegalArgumentException("Vertex already exists")
				else
					vertices = vertex :: vertices

				this
			}
			
			def removeVertex(vertex:T):Graph[T] = {
				if (!vertices.contains(vertex))
					throw new IllegalArgumentException("Vertex does not exist")
				else
				{
					// remove vertex
					vertices = vertices.filterNot(_ == vertex)

					// remove edges that have this vertex as a source or destination
					edges = edges.filterNot(e => e._1 == vertex || e._2 == vertex)
				}

				this
			}

			def addEdge(source:T, destination:T, weight:Int):Graph[T] = {
				if (!vertices.contains(source) || !vertices.contains(destination))
					throw new IllegalArgumentException("Vertex does not exist")
				else if (edgeExists(source, destination))
					throw new IllegalArgumentException("Edge already exists")
				else
				{
					edges = (source, destination, weight) :: edges
					if (!isDirected)
						edges = (destination, source, weight) :: edges
				}

				this
			}

			def removeEdge(source:T, destination:T):Graph[T] = {
				if (!vertices.contains(source) || !vertices.contains(destination))
					throw new IllegalArgumentException("Vertex does not exist")
				else if (!edgeExists(source, destination))
					throw new IllegalArgumentException("Edge does not exist")
				else
				{
					edges = edges.filterNot(e => e._1 == source && e._2 == destination)
					if (!isDirected)
						edges = edges.filterNot(e => e._1 == destination && e._2 == source)
				}

				// return this graph
				this
			}

			override def toString:String = {
				val sb = new StringBuilder
				sb.append("Vertices: ")
				vertices.foreach(v => sb.append(v + ", "))
				sb.append("\nEdges: ")
				edges.foreach(e => sb.append(e._1 + " -> " + e._2 + ", "))
				sb.toString
			}
		}
	}

	def main(args:Array[String])
	{
		//create an empty graph
		val undirectedGraph = Graph[String](false)

		//add some vertices of type String
		undirectedGraph.addVertex("A").addVertex("B").addVertex("C")

		//add some edges of type String
		undirectedGraph.addEdge("A", "B", 1).addEdge("A", "C", 2)

		//print the graph
		println(undirectedGraph)

		val directedGraph = Graph[String](true)

		directedGraph.addVertex("A").addVertex("B").addVertex("C")

		directedGraph.addEdge("A", "B", 1).addEdge("A", "C", 2)

		// remove a vertex
		directedGraph.removeVertex("B")

		println(directedGraph)
	}
}
