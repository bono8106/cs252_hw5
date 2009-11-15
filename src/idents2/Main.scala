package idents2

import java.io._
import java.util.Scanner
import java.util.concurrent.locks._
import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap
import scala.concurrent.ThreadRunner
import scala.concurrent.Lock

object Main {
  val result = new HashMap[String, TreeMap[String, Int]]

  val runner = new ThreadRunner()
  val resultLock = new Lock()
  var counter = 0
  val counterLock = new ReentrantLock()
  val counterDone = counterLock.newCondition

  def processToken(f: String, token : String) {
	  resultLock.acquire
	  try {
		  val fileMap = result.getOrElseUpdate(token, { new TreeMap[String, Int] })
		  val count = fileMap.getOrElse(f, 0)
		  result(token) = fileMap + (f -> (count + 1))
	  } finally {
		  resultLock.release
	   }
  }

  def processFile(f : File) {
    val in = new java.util.Scanner(f)
    in.useDelimiter("[^A-Za-z0-9_]+")
    while (in.hasNext) processToken(f.getPath, in.next)
  }

  def isInteresting(name : String) = name.endsWith(".java") || name.endsWith(".scala")

  def doProcessDirectory(dir : File) {
	  dir.listFiles foreach { f => 
	  if (f.isDirectory) {
		  counterLock.lock
		  try {
			  counter = counter + 1
		  } finally {
		 	  counterLock.unlock
		  }
		  runner execute { () => doProcessDirectory(f) }
      } else {
    	  if (isInteresting(f.getName)) 
    		  processFile(f)
      }
	  }
	  counterLock.lock
	  try {
		  counter = counter - 1
		  if (counter == 0)
		 	  counterDone.signal
	  } finally {
	 	  counterLock.unlock
	  }
  }
  
  def processDirectory(dir : File) {
	  counterLock.lock
	  try {
		  counter = 1
		  runner execute { () => doProcessDirectory(dir) }
		  counterDone.await
	  } finally {
	 	  counterLock.unlock
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
    processDirectory(new File(dirName))
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
