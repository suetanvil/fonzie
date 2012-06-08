/* Fonzie, Copyright (C) 2012 Chris Reuter, all rights reserved. */

package ca.blit.Fonzie

import scala.collection.mutable._
import scala.util.matching._

/** Container for method 'splitToWords'.  See it for details. */
private object Util {

  /**
   * This is the canonical word-splitter.  It is used whenever a listing
   * or product field needs to be split into multiple words.  It is here
   * so that there is a single, consistent splitting function.
   *
   * @param s    The string to split.
   * @return     The resulting array of strings.
   */

  def splitToWords(s:String) = s.split(" \t,".toArray)
}


/** Associates a string with one or more Product objects. */
private class WordMap {
  private val contents = Map[String, ListBuffer[Product]]()
  private var wordMax = 1

  /** Associates a(nother) Product with the string `key`.
   *
   *  @param    key     The key with which `value` will be associated.
   *  @param    value   The product associated with `key`.
   *
   *  @return           Unit
   */
  def update(key:String, value:Product) {
    contents.getOrElseUpdate(key, ListBuffer[Product]()).append(value)
    
    val ws = Util.splitToWords(key).size
    wordMax = ws max wordMax
  }

  /** Returns the maximum number of words (as determined by
   *  `Util.splitToWords`) that have been found in a key in `this` so far.
   *
   *  @return           The number of words.
   */
  def maxWordCount = wordMax

  /** Calls `action` once for each Product associated with `key` with the
   *  Product object as the argument.  Does nothing if there are no Products
   *  associated with `key`.
   *
   *  @param    key     The string with which the target Products are
   *                    associated.
   * 
   *  @param    action  The closure to call on each Product.
   * 
   *  @return           Unit
   */
  def getAndForeach(key:String) (action:(Product) => Unit) {
    val elements = contents.getOrElse(key, return)
    elements.foreach { elem => action(elem) }
  }
}



/** Instances of this class do the real work of matching listings to
 *  product descriptions.  The caller should first import all of the
 *  product descriptions via `addProduct`, then add the listings via
 *  `addListing`.  Results can then be retrieved using various public
 *  methods defined below.
 *
 *  @param      threshold       The minimum score a candidate Listing
 *                              must have to be considered a match.  The
 *                              score is always between 0.0 and 1.0.
 */
class RecordMatcher (
  val threshold:Double
) {
  // Results:
  private val rejects  = ArrayBuffer[Listing]()
  private val matches  = Map[Product,ArrayBuffer[Listing]]()
  
  // Reference data:
  private val models   = new WordMap    // Products by model name
  private val mparts   = new WordMap    // Products by model name words

  // If necessary, return an altered version of `p` with something
  // changed to make it easier to process later on.  This is a bit
  // hacky but the number of special cases is small.
  private def fixProductSpecialCases(p:Product):Product = {

    // Most manufacturers' names are a superset or subset of the
    // `manufacturer` field in a Listing, making them easy to match.
    // The only exception to this is Fujifilm, so we replace it with
    // something that does fit the pattern: 'fuji'.
    if (p.manufacturer.toLowerCase == "fujifilm") {
      return new Product(p.name, "fuji", p.family, p.model)
    }

    return p
  }

  /** Add a product `p` to this object's list of products.
   *
   *  @param    p       The product to add.
   */
  def addProduct(p:Product) {
    val prod = fixProductSpecialCases(p)

    models(prod.model.toLowerCase) = prod

    val model1 = """[a-z]+[0-9]+""".r
    val model2 = """[0-9]{3,}""".r
    for {prodPart <- Util.splitToWords(prod.model.toLowerCase)
         if (model1.findFirstIn(prodPart).isDefined || 
             model2.findFirstIn(prodPart).isDefined)
       } { 
         mparts(prodPart) = prod
       }
  }


  /** Instances of this class represent the possibility of associating
   *  a Listing with a Product.  They contain all of the information needed
   *  to compare the two and will compute a score between 0.0 and 1.0 (where
   *  higher is more likely to be a match).
   *
   *  @param    key             The model (fragment) used to find the product.
   *  @param    prod            The Product being evaluated for compatibility.
   *  @param    modelDistance   The distance in words from the expected
   *                            location of the model code in `desc` to its
   *                            actual location.
   *  @param    familyDistance  The distance in words from the expected
   *                            location of the product family name in `desc`
   *                            to its actual location or None if it does not
   *                            appear in `desc`.
   *  @param    mfgrDistance    The distance in words from the expected
   *                            location of the manufacturer's name in `desc`
   *                            to its actual location or None if it does not
   *                            appear in `desc`.
   *  @param    isSecondary     If true, means that `prod` was found by
   *                            searching for '''part''' of the model name
   *                            rather than the whole thing.
   *  @param    listing         The listing we are trying to match `prod` to.
   *  @param    desc            The word list searched to find `prod`.  This
   *                            is derived from `listing.title`.
   */
  private case class Candidate (
    val key:String,
    val prod:Product,
    val modelDistance:Int,
    val familyDistance:Option[Int],
    val mfgrDistance:Option[Int],
    val isSecondary:Boolean,
    val listing:Listing,
    val desc:Array[String]
  ) {
    /** The level of confidence that `listing` refers to `prod`, a number
     *  between 0.0 (not confident) and 1.0 (certain). */
    lazy val score = computeScore()

    // Compute the confidence score.
    private def computeScore():Double = {
      // Scoring parameters:

      // Importance of various subscores.  Must add up to 1.
      val DISTSCORE_RATIO  = 0.7
      val FAMILYDIST_RATIO = 0.15
      val KEYLEN_RATIO     = 0.15
      val PRIMARY_RATIO    = 0.1

      // Value for familyScore if there is no product family name
      // present in the listing.
      val NOFAMILY_SCORE   = 0.4 

      // Firstly, some deal-breakers:
      
      // Reject any item for which the manufacturer does not match the
      // manufacturer of prod.
      if (!mfgrMatches) return 0.0

      // If there's no manufacturer in the listing AND prod's
      // manufacturer isn't mentioned in the description, we reject
      // this outright.
      if (listing.manufacturer == "" && !mfgrDistance.isDefined) return 0.0

      // We also reject an entry if we only matched part of the model
      // number AND the family isn't given, since that's too little
      // information.
      if (!familyDistance.isDefined && isSecondary) return 0.0


      // Now, compute a (probably) non-zero score if we get this far.

      // Factor 1: how close is the word to the expected position?
      // Most descriptions start with the product manufacturer, family
      // and name so being close to the start is worth a lot.
      val distScore = distanceScore(modelDistance, desc.size)

      // Factor 2: Is the family name near the model name?
      val familyScore = 
        if (familyDistance.isDefined)
          distanceScore(familyDistance.get, desc.size)
        else if (listing.manufacturer == "")
          // If the listing has no manufacturer AND no family, we
          // assume a family name mismatch.  In this case, the match
          // is pretty dubious.
          0.0
        else
          // Otherwise, we give a reduced default.  We do this because
          // it's common for listings to not mention the family so we
          // don't want to penalize it too much.
          NOFAMILY_SCORE
          
      // Factor 3: How long (in words) is the matched key?  Longer
      // keys are, of course, more likely to to be a match.
      val keySize = Util.splitToWords(key).size
      val sizeScore = keySize.toDouble / models.maxWordCount
      
      // Factor 4: Did we match the whole model description or just a
      // significant fragment of it?
      val primaryScore = if (isSecondary) 0.0 else 1.0

      // And compute the final result:
      return (DISTSCORE_RATIO  * distScore      +
              FAMILYDIST_RATIO * familyScore    +
              KEYLEN_RATIO     * sizeScore      +
              PRIMARY_RATIO    * primaryScore)
    }

    // Compute a distance score (1.0 == at expected position, 0 ==
    // infinitely far away) for a positional distance of $distance in
    // a string of size $descSize.  If $descSize is short, we pad it
    // to MAXDIST_MIN.  This ups the score for small strings, since a
    // short distance there will get an unfairly low score due to the
    // string's short length.
    private def distanceScore (distance:Int, descLen:Int):Double = {
      val MAXDIST_MIN = 7
      val maxDist = descLen max (MAXDIST_MIN)
      return (maxDist - distance).abs.toDouble / maxDist
    }

    // Test if the manufacturers are similar enough that we can
    // consider them identical.  This usuall boils down to one being a
    // subset of the other (e.g. "kodak" and "eastman-kodak").
    // However, if the Listing's manufacturer field is blank, we
    // instead look at the first word of the description instead.
    private def mfgrMatches:Boolean = {
      var mfgr = if(listing.manufacturer != "") listing.manufacturer else desc(0)
      cmpMfgr(mfgr, prod.manufacturer)
    }

    // Test if s1 is a subset of s2 (case-insensitive) or vice-versa.
    private def cmpMfgr(s1:String, s2:String):Boolean = {
      if (s1.size > s2.size) return cmpMfgr(s2, s1)
      return s2.toLowerCase.containsSlice(s1.toLowerCase)
    }
  }


  /** Import a listing, attempt to associate it with a known Product or add
   *  to the list of rejected listings.
   *
   *  @param    l       The listing to match
   *  @return           Unit
   */
  def addListing(l:Listing) {
    candidate(l).foreach {
      c => {
        matches.
          getOrElseUpdate(c.prod, ArrayBuffer[Listing]()).
          append(l)
        return
      }
    }

    rejects.append(l)
  }

  // Return the best-matched Product for `l` or None if there is none.
  // The candidate '''must''' have a better score than `threshold`.
  private def candidate(l:Listing):Option[Candidate] =
    sortedCandidates(l).
      filter(_.score >= threshold).
      headOption

  // Return a list of Candidates for products matching `l` sorted from
  // highest score to lowest.
  private def sortedCandidates(l:Listing):List[Candidate] = {
    val desc = Util.splitToWords(l.title.toLowerCase).map{_.filterNot(_ == ',') }
    val elements = findCandidates(l,desc,false) ++ findCandidates(l,desc,true)
    return elements.sortWith{ (a,b) => a.score < b.score }.toList
  }

  // Search `desc` for a recognizable model number and return
  // Candidate objects for each Product that matches.
  private def findCandidates(l:Listing, desc:Array[String],
                             isSecondary:Boolean):Array[Candidate] = {
    // Constants: Expected positions in desc of the model number,
    // product family and manufacturer.
    val MODEL_POS = 2
    val FAMILY_POS = 1
    val MFGR_POS = 0

    val wordSet = if (isSecondary) mparts else models
    val result = new ArrayBuffer[Candidate]

    for {sz <- (1 to wordSet.maxWordCount).reverse } {
      for {ndx <- (0 to (desc.size - sz))} {
        val key = desc.slice(ndx, ndx + sz).reduceLeft(_+" "+_)

        wordSet.getAndForeach(key){
          elem => result.append(
            new Candidate(key, elem,
                          wordDistance(key, MODEL_POS, desc).get,
                          multiWordDistance(elem.family.toLowerCase,
                                            FAMILY_POS, desc),
                          multiWordDistance(elem.manufacturer.toLowerCase,
                                            MFGR_POS, desc),
                          isSecondary, l, desc))
        }
      }
    }

    return result.toArray
  }

  // Return the distance in words between the appearance of 'key' in
  // 'desc' from the expected position 'refPos'.  The count is always
  // non-negative.  If 'key' is not present in 'desc', returns None.
  private def wordDistance(key:String, refPos:Int, desc:Seq[String]):Option[Int] = {
    val keyWords = Util.splitToWords(key)

    val BAILOUT = desc.size + 100  // Sufficiently big number
    var dist = BAILOUT
    for {ndx <- 0 to (desc.size - keyWords.size)} {
      val cmpChunk = desc.slice(ndx, ndx + keyWords.size)
      if (keyWords.corresponds(cmpChunk)(_ == _))
        dist = dist min (ndx - refPos).abs
    }

    if (dist == BAILOUT) return None
    return Some(dist) 
  }

  // Like wordDistance but the key is split on whitespace and each
  // word is run through wordDistance in turn, with the closest match
  // being returned.
  private def multiWordDistance (keys:String, refPos:Int, desc:Seq[String]):Option[Int] = {
    val distances = Util.splitToWords(keys).
      map{ k => wordDistance(k, refPos, desc) }.
      filter {_.isDefined}.
      map{_.get}

    if (distances.isEmpty) return None
    
    return Some(distances.reduceLeft(_ min _))
  }

  /** Return the list of rejects as an array. */
  def rejectList = rejects.toArray

  /** Return the number of rejected Listings. */
  def rejectCount = rejects.size

  /** Return the list of matched objects.
   *
   *  @return           The list of matches as an array of MatchedResult.
   */
  def matchedResults:Array[MatchedResult] = {
    return matches.keys.toArray.
      sortWith{ (a, b) => a.name < b.name }.
      map{ key => MatchedResult(key.name, matches(key)) }
  }

  /** Return the number of matched items found. */
  def matchCount = matches.values.map{_.size}.fold(0){ (l, r) => l + r }

}


