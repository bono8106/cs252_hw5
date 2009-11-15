package idents1

import java.io._
import java.util.Scanner
import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap
import java.util.concurrent._
import java.util.concurrent.atomic._

object Main {
	val cresult = new ConcurrentHashMap[String, ConcurrentHashMap[String, AtomicInteger]]
   val result = new HashMap[String, TreeMap[String, Int]]

  def getWithDefault[K, V](map: ConcurrentHashMap[K, V], k: K, v: V) = {
	  val result = map.putIfAbsent(k, v)
	  if (result == null) v else result
  }

  def processToken(f: String, token : String) {
    val fileMap = getWithDefault(cresult, token, { new ConcurrentHashMap[String, AtomicInteger] })
    val count = getWithDefault(fileMap, f, new AtomicInteger(0))
    count.incrementAndGet
  }

  def processFile(f : File) {
    val in = new java.util.Scanner(f)
    in.useDelimiter("[^A-Za-z0-9_]+")
    while (in.hasNext) processToken(f.getPath, in.next)
  }

  def isInteresting(name : String) = name.endsWith(".java") || name.endsWith(".scala")

  def doProcessDirectory(dir : File, executor: Executor) {
	  dir.listFiles foreach { f => 
      if (f.isDirectory) 
        doProcessDirectory(f, executor) else 
        if (isInteresting(f.getName)) 
          executor.execute(new Runnable { def run { processFile(f) } })
    }
  }

  def processDirectory(dir : File) {
      val nThreads = Runtime.getRuntime.availableProcessors*2
      //System.err.println("Running " + nThreads)
	  val executor = new ThreadPoolExecutor(nThreads, nThreads, 
	 		  1000, TimeUnit.MILLISECONDS, 
	 		  new ArrayBlockingQueue[Runnable](nThreads * 10),
	 		  new ThreadPoolExecutor.CallerRunsPolicy)
      doProcessDirectory(dir, executor)
      executor.shutdown
      while (!executor.awaitTermination(1, TimeUnit.MINUTES)) {}
      
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
    
	  val it = cresult.entrySet.iterator 
      while (it hasNext) {
    	  val e = it.next
    	  var vv = new TreeMap[String, Int]
    	  val itit = e.getValue.entrySet.iterator
    	  while (itit.hasNext) { val x = itit.next; vv += (x.getKey -> x.getValue.get) }
    	  result(e.getKey) = vv
      }
      

    if (dump) {
    	val sorted: TreeMap[String, TreeMap[String, Int]] = new TreeMap[String, TreeMap[String, Int]] ++ result
    	
    	sorted foreach { case (key, value) => 
    		println(value.mkString("" + key + " {\n", "\n", "\n}"))
    	}
    }

    System.err.println("Milliseconds: " + (end - start))

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
