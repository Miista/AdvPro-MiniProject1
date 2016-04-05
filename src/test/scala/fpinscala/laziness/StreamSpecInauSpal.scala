// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecInauSpal extends FlatSpec with Checkers {
  import Stream._

  behavior of "take"

  it should "return an empty List when taking from an empty Stream" in {
    assert (empty.take (5).toList == List.empty)
  }

  it should "return the 10 first elements of an arbitrary Stream" in check {
    implicit def arbitraryIntList = Arbitrary[List[Int]] (genNonEmptyList[Int])
    implicit def intGen = Arbitrary[Int] (Gen.choose(0,200))

    ("take from stream" |: 
      Prop.forAll { (l: List[Int], n: Int) =>
        list2stream (l).take (n).toList == l.take (n) } ) && 
    ("take then stream" |:
      Prop.forAll { 
        (la: List[Int], n: Int) => list2stream (la).take (n).toList == list2stream (la.take (n)).toList } )
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for {
      la <- arbitrary[List[A]] suchThat (_.nonEmpty)
    } yield list2stream (la)

  def genNonEmptyList[A] (implicit arbA :Arbitrary[A]) :Gen[List[A]] =
    for {
      la <- arbitrary[List[A]] suchThat (_.nonEmpty)
    } yield la
}