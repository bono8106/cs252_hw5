package idents4

import java.io._
import java.util.Scanner
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinTask
import java.util.concurrent.RecursiveTask
import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap
import java.util.regex.Pattern

object Main {
  val delim = Pattern.compile("[^A-Za-z0-9_]+")

  type Result = HashMap[String, TreeMap[String, Int]]

	def processToken(result: Result, token : String, f: String, n: Int) {
		val fileMap = result.getOrElseUpdate(token, { new TreeMap[String, Int] })
		val count = fileMap.getOrElse(f, 0)
		result(token) = fileMap + (f -> (count + n))
	}

  abstract class ResultTask extends RecursiveTask[Result]
  
  class FileTask(f : File) extends ResultTask {
	  def compute = {
		  val result = new Result
		  val in = new java.util.Scanner(f).useDelimiter(delim)
	 	  while (in.hasNext) processToken(result, in.next, f.getPath, 1)
		  result
	   }
  }

  class DirectoryTask(dir : File) extends ResultTask {
	  implicit def arrayToJavaList[T](x: Array[T]) = java.util.Arrays.asList[T](x: _*)
	  implicit def javaCollectionToIterator[T](x: java.util.Collection[T]): Iterator[T] = {
		  val it = x.iterator
		  new Iterator[T] {
			  def hasNext = it.hasNext
			  def next = it.next
		   }
	   }

	  def isInteresting(name : String) = name.endsWith(".java") || name.endsWith(".scala")

	  def compute = {
	 	  ForkJoinTask.invokeAll(dir.listFiles map { f:File => if (f.isDirectory) new DirectoryTask(f) else if (isInteresting(f.getName)) new FileTask(f) else null } filter { _ != null }).foldLeft(new Result) { (a: Result, b: ResultTask) =>
	 	   		b.get foreach { case (token, fileMap) => 
		    		fileMap foreach { case (fileName, count) =>
		    			processToken(a, token, fileName, count)
		    		}
		    	}
		    	a
	 	   }
	  }
  }
	   
  def processDirectory(dir : File): Result = { 
      val nThreads = Runtime.getRuntime.availableProcessors
      val fjPool = new ForkJoinPool(nThreads)
      fjPool.invoke(new DirectoryTask(dir))
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
  }
}
