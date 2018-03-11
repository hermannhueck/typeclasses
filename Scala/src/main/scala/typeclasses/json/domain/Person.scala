package typeclasses.json.domain

import typeclasses.json.lib.JsonWriter.JsonWriterOps
import typeclasses.json.lib.{JsObject, JsonWriter}
import typeclasses.json.lib.JsonWriter.Instances._

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
