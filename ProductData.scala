/* Fonzie, Copyright (C) 2012 Chris Reuter, all rights reserved. */

package ca.blit.Fonzie

import scala.collection.mutable._
import scala.util.matching._

/** This is the abstract base class for all objects representing input or
 *  output data.  It also generates a JSON representation of a subinstances.
 *  (This is a bit hacky because I didn't want to require the end-user to
 *  have to install a third-party JSON generator.) */
abstract class ProductData {
  /** Return the list of field names associated with field values.  This is
   *  used to generated JSON. */
  def namesAndFields:List[(String, String)]

  /** Return the contents as JSON. */
  def asJSON = "{" + bodyJSON + "}"

  // Escape quote and backslash.
  private def escapeMeta(s:String) = "([\\\\\"])".r.replaceAllIn(s, "\\\\$1")
  
  // Return the contents as the inside of a JSON block.
  protected def bodyJSON = 
    namesAndFields.
      map {
        case (t:String, d:String) =>
          "\"" + t + "\":\"" + escapeMeta(d) + "\""
      }.
      reduceLeft(_ + "," + _)
}


/** Instances represent a single product as presented in the product list
 *  input file.
 *
 *  @param      name            The product's full name.
 *  @param      manufacturer    The name of the manufacturer
 *  @param      family          The name of the product family.
 *  @param      model           The name of the product model.
 */
case class Product (
  val name:String,
  val manufacturer:String,
  val family:String,
  val model:String
) extends ProductData {

  override def namesAndFields = 
    List(("name", name),
         ("manufacturer", manufacturer),
         ("family", family),
         ("model", model))
}


/** Instances represent a single product listing as present in the listings
 *  input file.
 *
 *  @param      title           The long description string.
 *  @param      manufacturer    The product's manufacturer.
 *  @param      currency        The currency of the price.
 *  @param      price           The price.
 */
case class Listing (
  val title:String,
  val manufacturer:String,
  val currency:String,
  val price:String
) extends ProductData {
  override def namesAndFields = 
    List(("title", title),
         ("manufacturer", manufacturer),
         ("currency", currency),
         ("price", price))
}


/** Instances represent an output record.  That is, they contain the name
 *  of a product (taken from the `name` field of a `Product`) followed by a
 *  list of the `Listing` objects that were successfully matched to the
 *  product.
 *
 *  @param      product_name    The unique name of the product.
 *  @param      productList     A list (Seq) of matching `Listing` objects.
 */
case class MatchedResult (
  val product_name:String,
  var productList:Seq[Listing]
) extends ProductData {
  override def namesAndFields = List( ("product_name", product_name) )
  override def asJSON =
    "{" + bodyJSON + ", \"listings\": [" + productListJSON + "]}"

  // Append the list of products to the JSON description.
  private def productListJSON():String = {
    if (productList.size == 0) return ""
    productList.reverse.map(_ asJSON).reduceLeft(_ + "," + _)
  }
}


