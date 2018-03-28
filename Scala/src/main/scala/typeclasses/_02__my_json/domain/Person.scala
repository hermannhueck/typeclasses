package typeclasses._02__my_json.domain

import typeclasses._02__my_json.libJson._
import JsonWriter.instances._
import JsonWriter.syntax._

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
