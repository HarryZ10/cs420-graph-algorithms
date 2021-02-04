import java.io.IOException

import scala.xml.XML.loadFile
import scala.xml.Node

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

		@throws(classOf[IllegalArgumentException])
		def getAdjacent(source:T):Iterable[T]

		def pathLength(path:Seq[T]):Option[Long]

		@throws(classOf[IllegalArgumentException])
		def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]]

		def getEdge(source:T, destination:T):Option[Edge[T]]

		def getEdges():Iterable[Edge[T]]

		def minimumSpanningTree:Option[Graph[T]]

		override def toString:String
	}	

	class Edge[T](val source:T, val destination:T, val weight:Int) extends Ordered[Edge[T]]
	{
		def compare(other:Edge[T]):Int = 0
		override def toString:String = ""
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
			def getVertices:Iterable[T] = Seq()

			def edgeExists(source:T, destination:T):Boolean = false
			
			def getEdgeWeight(source:T, destination:T):Option[Int] = None

			def addVertex(vertex:T):Graph[T] = null
			
			def removeVertex(vertex:T):Graph[T] = null

			def addEdge(source:T, destination:T, weight:Int):Graph[T] = null

			def removeEdge(source:T, destination:T):Graph[T] = null

			@throws(classOf[IllegalArgumentException])
			def getAdjacent(source:T):Iterable[T] = List()

			def pathLength(path:Seq[T]):Option[Long] = None

			@throws(classOf[IllegalArgumentException])
			def shortestPathBetween(source:T, destination:T):Option[Seq[Edge[T]]] = None

			@throws(classOf[IOException])
			def fromCSVFile(isDirected:Boolean, fileName:String):Graph[String] = null

			def getEdge(source:T, destination:T):Option[Edge[T]] = None

			def getEdges():Iterable[Edge[T]] = List()

			def minimumSpanningTree:Option[Graph[T]] = None

			override def toString:String = "Empty graph..."
		}

		/*
		Loads a graph from a TSP file
		*/
		def fromTSPFile(fileName:String):Graph[Int] =
		{
			//create an empty graph
			val emptyGraph = Graph[Int](false)

			//load the XML file
			val tspXML = loadFile(fileName)

			//get all the veritices
			val vertices = tspXML \\ "vertex"

			//add in all the vertices
			val graph = Range(0, vertices.size).foldLeft(emptyGraph)((g,v) => g.addVertex(v))

			//add in all the edges - they are part of each xml vertex
			vertices.zipWithIndex.foldLeft(graph)((g,t) => addXMLEdges(g, t._1, t._2))
		}

		/*
		Add in edges assume the vertices exist
		*/
		private def addXMLEdges(graph:Graph[Int], xmlEdges:Node, start:Int):Graph[Int] = 
		{
			//parse all the edges - tuples of (destination, weight)
			val edges = (xmlEdges \ "edge").map(e => (e.text.toInt, e.attributes("cost").text.toDouble.toInt))

			//remove the edges that already exist
			val newEdges = edges.filterNot(e => graph.edgeExists(start, e._1))

			//add in new edges
			newEdges.foldLeft(graph)((g,e) => g.addEdge(start, e._1, e._2))
		}
	}

	def main(args:Array[String])
	{
		//create an empty graph
		val exampleGraph = Graph[String](false)
		
		//print it out
		println(exampleGraph)
	}
}
