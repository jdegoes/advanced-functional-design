package net.degoes.afd.examples

object OpenTraits {

// both products

// this is more oop'ish
// open traits - con can be subclassed ()
  trait InputStream {
    def read(): Int
    def close(): Unit
  }

// this is more fp'ish
// case classes with functions inside
  final case class InputStream2(read: () => Int, close: () => Unit)

}
