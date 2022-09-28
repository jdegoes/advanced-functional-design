package net.degoes.afd.ruleengine.model

import zio._

object Examples extends ZIOAppDefault {
  val age    = FactDefinition.int("age")
  val name   = FactDefinition.string("name")
  val dob    = FactDefinition.instant("dob")
  val gender = FactDefinition.string("gender")

  val facts = Facts.empty

  val strAge: "age" = "age"

  // Recap scala 2.13.* like scala 3 -> singleton type is a type and a value
  val one: 1 = 1

  implicitly[1 <:< Int]
  implicitly["foo" <:< String]

  val facts2 = facts.add(age, 42).add(name, "John Doe").add(dob, java.time.Instant.now())

  val run =
    Console.printLine(facts2.get(age)) *>
      Console.printLine(facts2.get(name)) *>
      Console.printLine(facts2.get(dob)) /* *>
    Console.printLine(facts2.get(gender))*/ // Does not compile
}
