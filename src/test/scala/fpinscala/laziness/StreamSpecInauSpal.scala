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

  behavior of "map"

  def sumOfStream(s: Stream[Int]) = s.foldRight (0)(_+_)

  it should "sum correctly" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s: Stream[Int], n: Int) => 
        sumOfStream(s) * n == sumOfStream(s.map (_*n))
    }
  }

  it should "return itself when mapping with identity" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s: Stream[Int]) => s.map (identity).toList == s.toList }
  }

  behavior of "take"

  it should "return an empty List when taking from an empty Stream" in check {
    Prop.forAll { (n: Int) => empty.take (n).toList == List.empty }
  }

  it should "return the first elements of an arbitrary Stream" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def intGen = Arbitrary[Int] (Gen.choose(0,200))

    ("take from stream" |: 
      Prop.forAll { (s: Stream[Int], n: Int) =>
        s.take (n).toList == s.toList.take (n) } ) && 
    ("take then stream" |:
      Prop.forAll { 
        (s: Stream[Int], n: Int) => 
        s.take (n).toList == s.toList.take (n).toList } )
  }

  behavior of "drop"

  it should "return an empty List when dropping from an empty Stream" in check {
    Prop.forAll {
      (n: Int) => empty.drop (n).toList == List.empty
    }
  }

  it should "drop the same elements from a List" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def intGen = Arbitrary[Int] (Gen.choose(0,200))

    ("drop from stream" |: 
      Prop.forAll { (s: Stream[Int], n: Int) =>
        s.drop (n).toList == s.toList.drop (n) } )
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