package typeclasses._04b_my_json_oneimport.libJson

// A very simple JSON AST
//
sealed trait Json {
  def toJsonString: String = this match {
    case JsNull => "null"
    case JsBoolean(bool) => bool.toString
    case JsNumber(num) => num.toString
    case JsString(str) => "\"" + str + "\""
    case JsArray(elems) => elems.map(_.toJsonString).mkString("[", ", ", "]")
    case JsObject(bindings) => bindings map {
      case (key, value) => "\"" + key + "\": " + value.toJsonString
    } mkString("{", ", ", "}")
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
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

// The "serialize to JSON" behaviour is encoded in this trait.
// This is the type class, a trait with at least one type parameter.
//
trait JsonWriter[A] {
  def write(value: A): Json
}

// The type class companion object
//
object JsonWriter {

  // --- JsonWriter.apply[A] or JsonWriter[A]
  // makes the implicit JsonWriter[A] explicitly available
  //
  def apply[A: JsonWriter]: JsonWriter[A] = implicitly[JsonWriter[A]] // same as:
  // def apply[A](implicit writer: JsonWriter[A]): JsonWriter[A] = writer

  // --- type class instances for standard types
  //
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): Json = JsString(value)
  }

  implicit val booleanWriter: JsonWriter[Boolean] = (value: Boolean) => JsBoolean(value)

  implicit val doubleWriter: JsonWriter[Double] = (value: Double) => JsNumber(value)

  implicit val floatWriter: JsonWriter[Float] = (value: Float) => JsNumber(value.toDouble)

  implicit object longWriter extends JsonWriter[Long] {
    override def write(value: Long): Json = JsNumber(value.toDouble)
  }

  implicit def intWriter: JsonWriter[Int] = (value: Int) => JsNumber(value.toDouble)


  // --- generic instances

  // if you can write to Json an A, you can also write Option[A]
  //
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
    override def write(optA: Option[A]): Json = optA match {
      case None => JsNull
      case Some(a) => writer.write(a)
    }
  }

  // if you can write to Json an A, you can also write List[A]
  //
  implicit def listWriter[A](implicit writer: JsonWriter[A]): JsonWriter[List[A]] =
    (elems: List[A]) => JsArray(elems.map(writer.write))

  implicit def seqWriter[A: JsonWriter]: JsonWriter[Seq[A]] =
    (elems: Seq[A]) => JsArray(elems.toList.map(implicitly[JsonWriter[A]].write))

  implicit def mapWriter[A: JsonWriter]: JsonWriter[Map[String, A]] =
    (bindings: Map[String, A]) => JsObject(bindings.mapValues(JsonWriter[A].write))
}
