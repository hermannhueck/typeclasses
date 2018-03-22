package typeclasses._02_my_json.domain

import typeclasses._02_my_json.lib.JsonWriter.JsonWriterOps
import typeclasses._02_my_json.lib.{JsObject, JsonWriter}
import typeclasses._02_my_json.lib.JsonWriter.Instances._

final case class Person(name: String, married: Boolean, age: Int, email: String, children: Seq[String])

object Person {

  implicit val personWriter: JsonWriter[Person] = (p: Person) =>
    JsObject(Map(
      "name" -> p.name.toJson,
      "married" -> p.married.toJson,
      "age" -> p.age.toJson,
      "email" -> p.email.toJson,
      "children" -> p.children.toJson
    ))
}
