package typeclasses._02b_my_json_oneimport

package object libJson {

  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)

  // 4 ways to provide interface syntax as extension methods
  // The implicit class converts   A => JsonWriterOps[A]

  // 1. implicit class (with implicit method parameter)
  //
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]): Json = writer.write(value)
  }
  /*
  */

  // 2. implicit class (with context bound and implicitly)
  //
  /*
  implicit class JsonWriterOps[A: JsonWriter](value: A) {
    def toJson: Json = implicitly[JsonWriter[A]].write(value)
  }
  */

  // 3. implicit class (with context bound and apply)
  //
  /*
  implicit class JsonWriterOps[A: JsonWriter](value: A) {
    def toJson: Json = JsonWriter[A].write(value)
  }
  */

  /*
  // 4. Using an implicit conversion instead of an implicit class
  //
  import scala.language.implicitConversions

  implicit def convertToJsonWriterOps[A](value: A) = new {
    // 4a. using an implicit parameter
    def toJson(implicit writer: JsonWriter[A]): Json = writer.write(value)
  }
  implicit def convertToJsonWriterOps[A: JsonWriter](value: A) = new {
    // 4b. using context bound and implicitly
    def toJson: Json = implicitly[JsonWriter[A]].write(value)
  }
  implicit def convertToJsonWriterOps[A: JsonWriter](value: A) = new {
    // 4c. using context bound and apply
    def toJson: Json = JsonWriter[A].write(value)
  }
  */
}
