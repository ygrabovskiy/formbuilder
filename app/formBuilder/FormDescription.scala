package formBuilder


import play.api.data.{Mapping, Field, FormError, Form}
import scala._
import scala.AnyRef
import scala.Some
import play.api.i18n.{Messages, Lang}


case class FormExtension(

                          fieldsBuilders: Map[String, FieldExtension] = Map.empty,

                          fieldValues: Map[String, Seq[String]] = Map.empty,

                          convertValues: Map[String, Option[Any]] = Map.empty
                          )

class FormDescription[T](form: Form[T], val ext: FormExtension)(implicit private val mf: Manifest[T])
  extends Form[T](form.mapping, form.data, form.errors, form.value) {

  def fill(fieldValues: Map[String, Seq[String]]) = copyUnderlying(fieldValues = fieldValues)

  override def fill(value: T) = {


  }


  override def bindFromRequest()(implicit request: play.api.mvc.Request[_]): FormDescription[T] = {
    val data = (request.body match {
      case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      case _ => Map.empty[String, Seq[String]]
    }) ++ request.queryString

    copyUnderlying(
      fieldValues = data.map(m => (m._1, m._2)).filter(m => m._2.filter(v => !v.isEmpty).size > 0)
    )
  }


  // def apply(fieldFoo: T => Any): Field = apply(FieldNameGetter.$[T](fieldFoo))
  override def apply(key: String): Field = {

    val defValue = ext.fieldsBuilders.get(key).getOrElse {
      throw new RuntimeException(s"form description doesn't have $key field")
    }.attrs.get("_default")
    val value = ext.fieldValues.get(key) match {
      case Some(x) => x.headOption
      case _ => defValue.asInstanceOf[Option[Seq[String]]].getOrElse(Nil).headOption
    }

    val fieldsBuilder = ext.fieldsBuilders.get(key).getOrElse(throw new RuntimeException(s"field builder for key '$key' is not found"))
    val field = Field(form = this, name = key, constraints = Nil, format = None,
      errors = errors.collect {
        case e if e.key == key => e
      },
      value = value)
    new FieldDescription(this, field, fieldsBuilder)
  }

  def copyUnderlying(
                      fieldsBuilders: Map[String, FieldExtension] = ext.fieldsBuilders,
                      fieldValues: Map[String, Seq[String]] = ext.fieldValues,
                      convertValues: Map[String, Option[Any]] = ext.convertValues,
                      mapping: Mapping[T] = form.mapping,
                      data: Map[String, String] = form.data,
                      errors: Seq[FormError] = form.errors,
                      value: Option[T] = form.value
                      ) = new FormDescription[T](form.copy(mapping, data, errors, value), ext.copy(fieldsBuilders, fieldValues, convertValues))

}


case class FieldExtension(attrs: Map[String, Any], values: Option[Seq[String]])


class FieldDescription(form: Form[_], field: Field, val ext: FieldExtension)
  extends Field(form, field.name, field.constraints, field.format, field.errors, field.value) {


}