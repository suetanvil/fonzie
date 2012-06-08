/* Fonzie, Copyright (C) 2012 Chris Reuter, all rights reserved. */

package ca.blit.Fonzie

import scala.collection._
import java.io._
import com.twitter.json._

/** The main object for this program.  It contains the entry point and
 *  various utility functions.
 */
object sort {

  // Given a line of JSON, parse it and return the result as a Map.
  // Unparseable values result in a sufficiently unlikely Map that
  // will be rejected by the caller.  Parsing is done using Twitter's
  // open-source JSON parser.
  private def parseJSON(src:String):Map[String, String] = {
    Json.parse(src) match {
      case result:Map[String, String] => // Type erasure loses the subtypes,
        return result                    // but we can tolerate that

      case x =>
        println("got " + x toString)
        Map[String, String]("unexpected value" -> "boo!")
    }
  }

  // Read in the JSON file `filename` (as described below) and return
  // its contents as a List of Maps.
  private def readJSON(filename:String):List[Map[String, String]] = {
    try {
	  val src = io.Source.fromFile(filename).getLines
	  src.map{ (l:String) => parseJSON(l) }.toList
    } catch {
      case e:IOException =>
        println("IO error while reading file '" + filename + "':" + e.getMessage)
        sys.exit(1)
    }
  }

  // Read in the JSON file named by `filename`, one object at a time.
  // For each object, call `op` with an `Array` of `Strings` where
  // each element is the value in the object whose key is the
  // corresponding element in `keys`.
  //
  // The input file must consist of one JSON hash object per input
  // line.  If an object contains keys not in `keys`, it is ignored.
  private def collectJSON[T] (filename:String,
                              keys:List[String],
                              op:(Array[String]) => T
                            ) = {
    val knownKeys = keys.toSet

    def checkKeys(items:Map[String, String]) = {
      val knowAllKeys = items.keys.toSet.subsetOf(knownKeys)
      if (!knowAllKeys)
        println("Skipping invalid record in '" + filename + "', keys: " +
                items.keys.toString)

      knowAllKeys
    }

    for { item <- readJSON(filename) if checkKeys(item) }
    yield {
      val values = keys.map{ (k:String) => item.getOrElse(k, "") }.toArray
      op(values)
    }
  }

  // Read in the Products in the file `filename` and return them.
  private def readProducts(filename:String):List[Product] =
    collectJSON[Product](
      filename, 
      List("product_name", "manufacturer", "family", "model",
           "announced-date"),
      (v:Array[String]) => Product(v(0), v(1), v(2), v(3)))

  // Read in the Listings in the file `filename` and return them.
  private def readListings(filename:String):List[Listing] =
    collectJSON[Listing](
      filename, 
      List("title", "manufacturer", "currency", "price"),
      (v:Array[String]) => Listing(v(0), v(1), v(2), v(3)))

  // Save the data in `results` to the file named by `filename`.  On
  // error, be lame and just print a message. (I realize that this
  // isn't very robust, but it's good enough for a programming
  // challenge.)
  private def writeResults(filename:String, results:Seq[ProductData]) {
    try {
      val os = new FileWriter(filename)
      for {result <- results} {
        os.write(result.asJSON + "\n")
      }
      os.close()
    } catch {
      case e => println("Exception while writing '" + filename + "': "
                        + e toString)
    }
  }


  /** The entry point for this program. */
  def main (args:Array[String]) {

    // ----------------------------------------------------------------------
    // Parse the arguments

    // Flag values
    var threshold = 0.5
    
    def nextOption(args:List[String]):List[String] = {
      args match {
        case Nil => List[String]()

        case "--help" :: tail =>
          println("fonzie [--help] [--threshold <value>] <products>")
          println("    <listings> <results> [<rejections>]")
          sys.exit(0)
          nextOption(tail)      // Not called.
          
        case "--threshold" :: value :: tail =>
          threshold = value.toDouble
          nextOption(tail)

        case head :: tail => head :: nextOption(tail)
      }
    }

    val parms = nextOption(args.toList)
    if (parms.length != 3 && parms.length != 4) nextOption(List("--help"))

    val productFile = parms(0)
    val listingFile = parms(1)
    val resultFile  = parms(2)
    val rejectFile  = if(parms.length >= 4) parms(3) else ""

    // ----------------------------------------------------------------------
    // Read in the data files.

    println("Reading " + productFile + "...")
    val products = readProducts(productFile)

    println("Reading " + listingFile + "...")
    val listings = readListings(listingFile)

	println("Found " + products.size + " products and " + listings.size +
			" listings.")

    // ----------------------------------------------------------------------
    // Create the matcher and insert the data

    val matcher = new RecordMatcher(threshold)

    println("Inserting products...")
    for {p <- products} matcher.addProduct(p)

    println("Processing listings...")
    for {l <- listings} matcher.addListing(l)

    // ----------------------------------------------------------------------
    // Display the resulting statistics

    val matchedResults = matcher.matchedResults
    val matchCount = matcher.matchCount
    val matchPercentage = matchCount.toDouble*100 / listings.size.toDouble
    println("Matched %d items of %d (%.2f%%).".
            format(matchCount, listings.size, matchPercentage))
    
    // ----------------------------------------------------------------------
    // Save the results

    println("Writing results...")
    writeResults(resultFile, matchedResults)

    if (rejectFile != "") {
      println("Writing rejects...")
      writeResults(rejectFile, matcher.rejectList)
    }

  }
}




