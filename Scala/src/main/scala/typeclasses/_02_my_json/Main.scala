package typeclasses._02_my_json

import typeclasses._02_my_json.domain.Person
import typeclasses._02_my_json.domain.Person.personWriter
import typeclasses._02_my_json.lib.{JsNull, Json}
import typeclasses._02_my_json.lib.JsonWriter.Instances._
import typeclasses._02_my_json.lib.JsonWriter.JsonWriterOps

object Main extends App {

  private def printJson(json: Json): Unit = println(s"$json\n   --   ${json.stringify}")

  val john = Person("John", true, 34, "john@example.com", List("Helen", "Carlie", "Maria"))
  val dave = Person("Dave", false, 45, "dave@example.com", List())

  println("\n===== Using interface object with primitive Json values ...")

  printJson(Json.toJson("a String"))
  printJson(Json.toJson(42))
  printJson(Json.toJson(false))
  printJson(JsNull)

  println("\n===== Using interface object with Option[String] ...")

  val jsOptionalStr = Json.toJson(Option("some String"))(optionWriter(stringWriter))
  printJson(jsOptionalStr)
  val jsOptionalNullStr = Json.toJson[Option[String]](Option(null))
  printJson(jsOptionalNullStr)
  val jsSomeStr = Json.toJson(Some("some String"))(someWriter)
  printJson(jsSomeStr)
  val jsSomeStr2 = Json.toJson(Some("some String"))
  printJson(jsSomeStr2)
  val jsNoString = Json.toJson[Option[String]](None)
  printJson(jsNoString)

  println("\n===== Using interface object with Person and Option[Person] ...")

  val jsJohn = Json.toJson(john)(personWriter)
  printJson(jsJohn)
  val jsDave = Json.toJson(dave)
  printJson(jsDave)
  val jsSomeDave = Json.toJson(Some(dave))
  printJson(jsSomeDave)
  val jsNoPerson = Json.toJson[Option[Person]](None)
  printJson(jsNoPerson)

  println("\n===== Using interface syntax with String and Option[String] ...")

  val jsStr2 = "another String".toJson(stringWriter)
  printJson(jsStr2)
  val jsStr3 = "yet another String".toJson
  printJson(jsStr3)
  val jsSomeStr3 = Some("yet another String").toJson
  printJson(jsSomeStr3)
  val jsNoString2 = None.asInstanceOf[Option[String]].toJson
  printJson(jsNoString2)

  println("\n===== Using interface syntax with Person and Option[Person] ...")

  val jsJohn2 = john.toJson(personWriter)
  printJson(jsJohn2)
  val jsDave2 = dave.toJson
  printJson(jsDave2)
  val jsSomeDave2 = Some(dave).toJson
  printJson(jsSomeDave2)
  val jsNoPerson2 = None.asInstanceOf[Option[Person]].toJson
  printJson(jsNoPerson2)

  println("\n=====\n")
}
