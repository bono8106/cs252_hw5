package idents3

import java.io._
import java.util.Scanner
import scala.collection._
import scala.collection.mutable.Queue
import scala.collection.immutable.TreeMap
import scala.actors._
import scala.actors.Actor._

object Main {

	val assert = false
	val debug = false
	
	type Result = immutable.HashMap[String, TreeMap[String, Int]]
	type MResult = mutable.HashMap[String, TreeMap[String, Int]]
	
	def deepCount(map: Map[String, Map[String, Int]]) = 
		map.mapValues( _.values.foldLeft(0) (_+_) ).values.foldLeft(0) (_+_)
	
	def processToken(result: MResult, token : String, f: String, n: Int) {
		val fileMap = result.getOrElseUpdate(token, { new TreeMap[String, Int] })
		val count = fileMap.getOrElse(f, 0)
		result(token) = fileMap + (f -> (count + n))
	}

	def isInteresting(name : String) = name.endsWith(".java") || name.endsWith(".scala")

	case class ReducerDone(result: Result)
	case class ComputerDone(result: Result)

	// Compute phase
	class FileActor(mergeDispatch: Actor) extends Actor {
		def act() {
			react {
				case f : File =>
					var result = new MResult
					val in = new java.util.Scanner(f)
					in.useDelimiter("[^A-Za-z0-9_]+")
					var count = 0
					while (in.hasNext) { processToken(result, in.next, f.getPath, 1); count+=1 }
					if (assert) if (count != deepCount(result)) System.err.println("error " + deepCount(result) + " != " + count)
					mergeDispatch ! (new Result ++ result)
			}
		}
	}

	// Reduce phase
	class MergeActor extends Actor {
		def act() {
			react {
				case (a: Result, b: Result) =>
					val result = new MResult
					result ++= a
			    	b foreach { case (token, fileMap) => 
			    		fileMap foreach { case (fileName, count) =>
			    			processToken(result, token, fileName, count)
			    		}
			    	}
					
					if (assert) {
						val ac = deepCount(a)
						val bc = deepCount(b)
						val rc = deepCount(result)
						if (rc != (ac+bc)) System.err.println("error " + rc + " != " + ac + " + " + bc)
					}
					
					sender ! (new Result ++ result)
			}
		}
	}

	case class MapperMsg
	case class AddMapper(dir : File) extends MapperMsg
	case class MapperDone(dir : File) extends MapperMsg

	// Reduce phase coordinator and progress tracker
	class MergeDispatchActor(parent: Actor) extends Actor {
		var totalMappers = 1 // counts the total number of directories
		var completedMappers = 0 // counds the number of finished directories
		var totalReducers = 0
		var completedReducers = 0
		val resultQueue = new Queue[Result]

		def act() {
			// Seed queue - allows completion when no files are found at the cost of an extra merge
			resultQueue enqueue new Result
			loop {
				react {
					// Reduce
					case partialResult: Result => 
						resultQueue enqueue partialResult
						if (sender.isInstanceOf[MergeActor]) {
							completedReducers += 1
							checkIfDone
						}
			 	  	  	while (resultQueue.length >= 2) {
			 	  	  		val merger = new MergeActor().start
			 	  	  		val a = resultQueue.dequeue
			 	  	  		val b = resultQueue.dequeue
			 	  	  		merger ! (a, b)
			 	  	  		totalReducers += 1
			 	  	  		if (debug) System.err.println("Completed reducers " + completedReducers + " of " + totalReducers)
			 	  	  	}
					// Progress tracking
					case _: AddMapper =>
						totalMappers += 1
					case _: MapperDone =>
						completedMappers += 1
						if (debug) System.err.println("Completed mappers " + completedMappers + " of " + totalMappers)
						checkIfDone
				}
			}
		}

		def checkIfDone {
			if (resultQueue.length == 1 && completedMappers == totalMappers && completedReducers == totalReducers) {
				parent ! resultQueue.dequeue
				this.exit // terminate this Actor. qualify to explicitly disambiguate from Predef.exit
			}
		}
	}

	// Map phase
	class DirectoryActor(mergeDispatch: Actor) extends Actor {
		def act() {
			react {
				case dir : File =>
					dir.listFiles foreach { f => 
						if (f.isDirectory) {
							mergeDispatch ! new AddMapper(f) // one more directory to process before completion
							new DirectoryActor(mergeDispatch).start ! f
						} else {
							if (isInteresting(f.getName)) {
								new FileActor(mergeDispatch).start ! f
							}
						}
					}
					mergeDispatch ! new MapperDone(dir) // done with this directory
			}
		}
	}

	def processDirectory(dir : File) = {
		val mergeDispatch = new MergeDispatchActor(self).start
		new DirectoryActor(mergeDispatch).start ! dir
		self receive {
			case result: Result => result
		}
	}

  def main(args: Array[String]) {
    var dump = false
    var dirName = ""
    if (args.length == 2) {
    	if (args(0) == "-d") {
    		dump = true
    		dirName = args(1)
    	} else {
    		throw new RuntimeException("Invalid arguments")
    	}
    } else if (args.length == 1) {
    	dirName = args(0)
    	dump = false
    } else {
    	throw new RuntimeException("Invalid argumnets")
    }

    val start = System.currentTimeMillis
    val result = processDirectory(new File(dirName))
    val end = System.currentTimeMillis
    
    System.err.println("Milliseconds: " + (end - start))

    if (dump) {
    	val sorted = new TreeMap[String, TreeMap[String, Int]] ++ result
    	sorted foreach { case (key, value) => 
    		println(value.mkString("" + key + " {\n", "\n", "\n}"))
    	}
    }

    if (!dump) {
	    print("Enter identifier: ")
	    val in = new Scanner(System.in)
	    while (in.hasNext) {
	      val ident = in.next
	      if (result.contains(ident)) {
	        println(result(ident).mkString("\n"))
	        println("Total count: " + (result(ident).values.foldLeft(0) (_+_)))
	      } else {
	          println("Not found")
	      }
	      print("Enter identifier: ")
	    }
	    println()
    }
  }
}
