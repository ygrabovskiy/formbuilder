package fieldBuilder

import java.util

trait FieldBuilder[T] {
  type Self

  def addProperty(key: String, value: Any): Self
  def addToCollection(key: String, value: Any): Self
  def getProperty(key: String): Option[Any]

}

case class MapFieldBuilder[T](propertyMap: Map[String, Any]) extends FieldBuilder[T] {
  type Self = MapFieldBuilder[T]

  def addProperty(key: String, value: Any) = copy(propertyMap = propertyMap + (key -> value) )

  def addToCollection(key: String, value: Any) = addProperty(key, getProperty(key).map(_.asInstanceOf[Seq] :+ value).getOrElse(Seq()) )

  def getProperty(key: String): Option[Any]   = propertyMap.get(key)
}

object FieldBuilderExtenders {
   implicit class StringFieldBuilderExtender(fb: FieldBuilder[String]) {
     def maxSize(s: Int) = fb.addProperty("maxSize", s)
   }
}

object FieldBuilderUser{
  import FieldBuilderExtenders.StringFieldBuilderExtender
  def foo () = {
    val fb = MapFieldBuilder[String](Map())
    fb.maxSize(3)

  }
}
