package typeclasses._02b_my_json_oneimport.domain

import typeclasses._02b_my_json_oneimport.libJson._

final case class Person(name: String, married: Boolean, age: Int, email: String, children: Seq[String])

object Person {

  // import scala.language.reflectiveCalls

  implicit val personWriter: JsonWriter[Person] = (p: Person) =>
    JsObject(Map(
      "name" -> p.name.toJson,
      "married" -> p.married.toJson,
      "age" -> p.age.toJson,
      "email" -> p.email.toJson,
      "children" -> p.children.toJson
    ))
}
