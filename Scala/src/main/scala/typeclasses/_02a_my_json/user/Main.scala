package typeclasses._02a_my_json.user

import typeclasses._02a_my_json.domain.Person
import typeclasses._02a_my_json.libJson._

object Main extends App {

  private def printJson(json: Json): Unit = println(s"$json\n   --   ${json.toJsonString}")

  val john = Person("John", true, 34, "john@example.com", List("Helen", "Charlie", "Maria"))
  val dave = Person("Dave", false, 45, "dave@example.com", List())

  {
    println("\n----- Using interface object with primitive values ...")

    import JsonWriter.instances._

    printJson(Json.toJson("a String"))
    printJson(Json.toJson(42))
    printJson(Json.toJson(false))
    printJson(JsNull)
  }

  {
    println("\n----- Using interface object with Option[String] ...")

    import JsonWriter.instances._

    val jsSomeStr = Json.toJson(Option("some String"))
    printJson(jsSomeStr)
    val jsSomeStr2 = Json.toJson[Option[String]](Some("some String")) // avoid Some.apply, use Option.apply instead
    printJson(jsSomeStr2)
    val jsNoString = Json.toJson(Option.empty[String])
    printJson(jsNoString)
    val jsNoString2 = Json.toJson[Option[String]](None) // avoid None, use Option.empty instead
    printJson(jsNoString2)
  }

  {
    println("\n----- Using interface object with Person and Option[Person] ...")

    import JsonWriter.instances._
    import Person._

    val jsDave = Json.toJson(dave)
    printJson(jsDave)
    val jsSomeDave = Json.toJson(Option(dave))
    printJson(jsSomeDave)
    val jsSomeDave2 = Json.toJson[Option[Person]](Some(dave)) // avoid Some.apply, use Option.apply instead
    printJson(jsSomeDave2)
    val jsNoPerson = Json.toJson(Option.empty[Person])
    printJson(jsNoPerson)
    val jsNoPerson2 = Json.toJson[Option[Person]](None) // avoid None, use Option.empty instead
    printJson(jsNoPerson2)
  }

  {
    println("\n----- Using interface object with List[A] ...")

    import JsonWriter.instances._
    import Person._

    val ints = Json.toJson(List(1, 2, 3, 4, 5))
    printJson(ints)
    val noInts = Json.toJson(List.empty[Int])
    printJson(noInts)
    val jsPersons = Json.toJson(List(john, dave))
    printJson(jsPersons)
    val jsNoPersons = Json.toJson(List.empty[Person])
    printJson(jsNoPersons)
  }

  {
    println("\n----- Using interface syntax with primitive values ...")

    import JsonWriter.instances._
    import JsonWriter.syntax._

    printJson("a String".toJson)
    printJson(42.toJson)
    printJson(false.toJson)
    printJson(JsNull)
  }

  {
    println("\n----- Using interface syntax with String and Option[String] ...")

    import JsonWriter.instances._
    import JsonWriter.syntax._

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

    import JsonWriter.instances._
    import JsonWriter.syntax._
    import Person._

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

    import JsonWriter.instances._
    import JsonWriter.syntax._
    import Person._

    printJson(List(1, 2, 3, 4, 5).toJson)
    printJson(List(john, dave).toJson)
    printJson(List.empty[Person].toJson)
  }

  println("\n-----\n")
}
