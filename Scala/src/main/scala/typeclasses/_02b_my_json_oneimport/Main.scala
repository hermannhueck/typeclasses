package typeclasses._02b_my_json_oneimport

import typeclasses._02b_my_json_oneimport.domain.Person
import typeclasses._02b_my_json_oneimport.libJson._

object Main extends App {

  private def printJson(json: Json): Unit = println(s"$json")
  //private def printJson(json: Json): Unit = println(s"$json\n   --   ${json.toJsonString}")

  val john = Person("John", true, 34, "john@example.com", List("Helen", "Charlie", "Maria"))
  val dave = Person("Dave", false, 45, "dave@example.com", List())

  {
    println("\n----- Using interface object with primitive values ...")

    printJson(Json.toJson("a String"))
    printJson(Json.toJson(42))
    printJson(Json.toJson(false))
    printJson(JsNull)
  }

  {
    println("\n----- Using interface object with Option[String] ...")

    val jsSomeStr = toJson(Option("some String"))
    printJson(jsSomeStr)
    val jsSomeStr2 = toJson[Option[String]](Some("some String")) // avoid Some.apply, use Option.apply instead
    printJson(jsSomeStr2)
    val jsNoString = toJson(Option.empty[String])
    printJson(jsNoString)
    val jsNoString2 = toJson[Option[String]](None) // avoid None, use Option.empty instead
    printJson(jsNoString2)
  }

  {
    println("\n----- Using interface object with Person and Option[Person] ...")

    val jsDave = toJson(dave)
    printJson(jsDave)
    val jsSomeDave = toJson(Option(dave))
    printJson(jsSomeDave)
    val jsSomeDave2 = toJson[Option[Person]](Some(dave)) // avoid Some.apply, use Option.apply instead
    printJson(jsSomeDave2)
    val jsNoPerson = toJson(Option.empty[Person])
    printJson(jsNoPerson)
    val jsNoPerson2 = toJson[Option[Person]](None) // avoid None, use Option.empty instead
    printJson(jsNoPerson2)
  }

  {
    println("\n----- Using interface object with List[A] ...")

    val ints = toJson(List(1, 2, 3, 4, 5))
    printJson(ints)
    val noInts = toJson(List.empty[Int])
    printJson(noInts)
    val jsPersons = toJson(List(john, dave))
    printJson(jsPersons)
    val jsNoPersons = toJson(List.empty[Person])
    printJson(jsNoPersons)
  }

  // import scala.language.reflectiveCalls

  {
    println("\n----- Using interface syntax with primitive values ...")

    printJson("a String".toJson)
    printJson(42.toJson)
    printJson(false.toJson)
    printJson(JsNull)
  }

  {
    println("\n----- Using interface syntax with String and Option[String] ...")

    val jsStr = "yet another String".toJson
    printJson(jsStr)
    val jsSomeStr = Option("yet another String").toJson
    printJson(jsSomeStr)
    val jsSomeStr2 = Some("yet another String").asInstanceOf[Option[String]].toJson // avoid Some.apply, use Option.apply instead
    printJson(jsSomeStr2)
    val jsNoString = Option.empty[String].toJson
    printJson(jsNoString)
    val jsNoString2 = None.asInstanceOf[Option[String]].toJson // avoid None, use Option.empty instead
    printJson(jsNoString2)
  }

  {
    println("\n----- Using interface syntax with Person and Option[Person] ...")

    val jsDave = dave.toJson
    printJson(jsDave)
    val jsSomeDave = Option(dave).toJson
    printJson(jsSomeDave)
    val jsSomeDave2 = Some(dave).asInstanceOf[Option[Person]].toJson // avoid Some.apply, use Option.apply instead
    printJson(jsSomeDave2)
    val jsNoPerson = Option.empty[Person].toJson
    printJson(jsNoPerson)
    val jsNoPerson2 = None.asInstanceOf[Option[Person]].toJson // avoid None, use Option.empty instead
    printJson(jsNoPerson2)
  }

  {
    println("\n----- Using interface syntax with List[A] ...")

    printJson(List(1, 2, 3, 4, 5).toJson)
    printJson(List(john, dave).toJson)
    printJson(List.empty[Person].toJson)
  }

  println("\n-----\n")
}
