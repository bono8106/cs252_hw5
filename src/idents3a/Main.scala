package idents3a

import java.io._
import java.util.Scanner
import scala.collection._
import scala.collection.mutable.Queue
import scala.collection.immutable.TreeMap
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.dispatch.Promise
import akka.dispatch.Await
import akka.util.duration._
import akka.util.Timeout

object Main {
  
  val actorSystem = ActorSystem("rocket")

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

	// Compute phase
	class FileActor extends Actor {
		def receive = {
			case f : File =>
				var result = new MResult
				val in = new java.util.Scanner(f)
				in.useDelimiter("[^A-Za-z0-9_]+")
				var count = 0
				while (in.hasNext) { processToken(result, in.next, f.getPath, 1); count+=1 }
				if (assert) if (count != deepCount(result)) System.err.println("error " + deepCount(result) + " != " + count)
				sender ! (result)
				context.stop(self)
		}
	}

	case class AddComputer(f : File)

	// Reduce phase coordinator and progress tracker
	class MergeDispatchActor(parent: Promise[MResult]) extends Actor {
		var workingComputers = 0 // counts the total number of directories
		val result = new MResult

		def receive = {
			// Reduce
			case partialResult: MResult => 
		    	partialResult foreach { case (token, fileMap) => 
		    		fileMap foreach { case (fileName, count) =>
		    			processToken(result, token, fileName, count)
		    		}
		    	}
		    	workingComputers -= 1
		    	
				if (workingComputers == 0) {
					parent.success(result)
					context.stop(self) // terminate this Actor. qualify to explicitly disambiguate from Predef.exit
				}
			// Progress tracking
			case AddComputer(file) =>
				workingComputers += 1
				context.actorOf(Props[FileActor]) ! file
		}

	}

  def doProcessDirectory(dir : File, mergeDispatch: ActorRef) {
	  dir.listFiles foreach { f => 
      if (f.isDirectory) 
        doProcessDirectory(f, mergeDispatch) else 
        if (isInteresting(f.getName)) 
          mergeDispatch ! new AddComputer(f)
    }
  }

	def processDirectory(dir : File): MResult = {
	  val promise = Promise[MResult]()(actorSystem.dispatcher)
		val mergeDispatch = actorSystem.actorOf(Props(new MergeDispatchActor(promise)), "merge-dispatch")
		doProcessDirectory(dir, mergeDispatch)
		Await.result(promise, Timeout(5 minutes).duration)
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
    	System.err.println("Milliseconds+sort: " + (System.currentTimeMillis - start))
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
    
    actorSystem.shutdown()
  }
}
