package typeclasses.json

import typeclasses.json.domain.Person
import typeclasses.json.domain.Person.personWriter
import typeclasses.json.lib.{JsNull, Json}
import typeclasses.json.lib.JsonWriter.Instances._
import typeclasses.json.lib.JsonWriter.JsonWriterOps

object Main extends App {

  private def showJson(json: Json): Unit = println(s"$json\n   --   ${json.stringify}")

  val john = Person("John", true, 34, "john@example.com", List("Helen", "Carlie", "Maria"))
  val dave = Person("Dave", false, 45, "dave@example.com", List())

  println("\n===== Using interface object with primitive Json values ...")

  showJson(Json.toJson("a String"))
  showJson(Json.toJson(42))
  showJson(Json.toJson(false))
  showJson(JsNull)

  println("\n===== Using interface object with Option[String] ...")

  val jsOptionalStr = Json.toJson(Option("some String"))(optionWriter(stringWriter))
  showJson(jsOptionalStr)
  val jsOptionalNullStr = Json.toJson[Option[String]](Option(null))
  showJson(jsOptionalNullStr)
  val jsSomeStr = Json.toJson(Some("some String"))(someWriter)
  showJson(jsSomeStr)
  val jsSomeStr2 = Json.toJson(Some("some String"))
  showJson(jsSomeStr2)
  val jsNoString = Json.toJson[Option[String]](None)
  showJson(jsNoString)

  println("\n===== Using interface object with Person and Option[Person] ...")

  val jsJohn = Json.toJson(john)(personWriter)
  showJson(jsJohn)
  val jsDave = Json.toJson(dave)
  showJson(jsDave)
  val jsSomeDave = Json.toJson(Some(dave))
  showJson(jsSomeDave)
  val jsNoPerson = Json.toJson[Option[Person]](None)
  showJson(jsNoPerson)

  println("\n===== Using interface syntax with String and Option[String] ...")

  val jsStr2 = "another String".toJson(stringWriter)
  showJson(jsStr2)
  val jsStr3 = "yet another String".toJson
  showJson(jsStr3)
  val jsSomeStr3 = Some("yet another String").toJson
  showJson(jsSomeStr3)
  val jsNoString2 = None.asInstanceOf[Option[String]].toJson
  showJson(jsNoString2)

  println("\n===== Using interface syntax with Person and Option[Person] ...")

  val jsJohn2 = john.toJson(personWriter)
  showJson(jsJohn2)
  val jsDave2 = dave.toJson
  showJson(jsDave2)
  val jsSomeDave2 = Some(dave).toJson
  showJson(jsSomeDave2)
  val jsNoPerson2 = None.asInstanceOf[Option[Person]].toJson
  showJson(jsNoPerson2)

  println("\n=====\n")
}
