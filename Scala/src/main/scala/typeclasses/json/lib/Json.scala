package typeclasses.json.lib

// A very simple JSON AST
//
sealed trait Json {
  def stringify: String = this match {
    case JsNull => "null"
    case JsBoolean(bool) => bool.toString
    case JsNumber(num) => num.toString
    case JsString(str) => "\"" + str + "\""
    case JsArray(elems) => elems.map(_.stringify).mkString("[", ", ", "]")
    case JsObject(bindings) => bindings.map {
      case (key, value) => "\"" + key + "\": " + value.stringify
    }.mkString("{", ", ", "}")
  }
}

final case object JsNull extends Json
final case class JsBoolean(bool: Boolean) extends Json
final case class JsString(str: String) extends Json
final case class JsNumber(num: Double) extends Json
final case class JsArray(elems: List[Json]) extends Json
final case class JsObject(bindings: Map[String, Json]) extends Json


// Json companion object
//
object Json {

  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

// The "serialize to JSON" behaviour is encoded in this trait.
// This is the type class, a trait with at least one type parameter.
//
trait JsonWriter[A] {
  def write(value: A): Json
}

// The type classes comnpanion object
//
object JsonWriter {

  // extension method toJson for any type for which an implicit writer instance is in scope
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]): Json =
      writer.write(value)
  }

  // --- type class instances for standard types
  object Instances {

    implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

    implicit val booleanWriter: JsonWriter[Boolean] = (value: Boolean) => JsBoolean(value)

    implicit val doubleWriter: JsonWriter[Double] = (value: Double) => JsNumber(value)

    implicit val floatWriter: JsonWriter[Float] = (value: Float) => JsNumber(value.toDouble)

    implicit val longWriter: JsonWriter[Long] = (value: Long) => JsNumber(value.toDouble)

    implicit val intWriter: JsonWriter[Int] = (value: Int) => JsNumber(value.toDouble)

    // unfortunately works only for Option[A], not for Some[A] or None.type
    // the reason is that JsonWriter[A] is invariant (could be covariant with other drawbacks)
    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
      case None => JsNull
      case Some(value) => writer.write(value)
    }

    implicit def someWriter[A](implicit optionWriter: JsonWriter[Option[A]]): JsonWriter[Some[A]] =
      optionWriter.asInstanceOf[JsonWriter[Some[A]]] // someWriter is the optionWriter cast to JsonWriter[Some[A]]

    implicit def noneWriter[A](implicit optionWriter: JsonWriter[Option[A]]): JsonWriter[None.type] =
      optionWriter.asInstanceOf[JsonWriter[None.type]] // noneWriter is the optionWriter cast to JsonWriter[None.type]
  }

  implicit def seqWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Seq[A]] =
    (elems: Seq[A]) => JsArray(elems.toList.map(writer.write))

  implicit def mapWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Map[String, A]] =
    (bindings: Map[String, A]) => JsObject(bindings.mapValues(writer.write))
}
