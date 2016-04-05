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

// import stream00._    // uncomment to test the book solution
import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecInauSpal extends FlatSpec with Checkers {
  import Stream._

  behavior of "headOption"

  it should "not force the tail of the stream" in {
    // val s = Stream.cons(println("hej"), Stream.cons(println("med"), Stream.cons(println("dig"), Stream.empty[Unit])))
    // s.headOption
    // s match {
    //   case Empty => println("lol")
    //   case Cons(h,t) => {
    //     println("t:"+ isTailEval(s))
    //   }
    // }
    // println("s:"+s)
  }

  def isTailEval[A](s: Stream[A]): Boolean =
    s match {
      case Empty => false
      case Cons(h,t) => tailOfStream(s) == t
    }

  behavior of "append"

  it should "return the same sum of two appended Streams" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll {
      (s1: Stream[Int], s2: Stream[Int]) => {
        sumOfStream (s1.append (s2)) == sumOfStream (s1) + sumOfStream (s2) }
    }
  }

  behavior of "map"

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

  val intStreams = genNonEmptyStream[Int]
  val positiveInts = arbitrary[Int].suchThat (0 < _)

  it should "respect idempotency" in check {
    Prop.forAll(intStreams, positiveInts) { (s, n) => 
      s.take(n).take(n).toList == s.take (n).toList
    }
  }

  behavior of "drop"

  it should "return an empty List when dropping from an empty Stream" in check {
    Prop.forAll {
      (n: Int) => empty.drop (n).toList == List.empty }
  }

  it should "respect additivity" in check {
    implicit def arbitraryStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def intGen = Arbitrary[(Int,Int)] (
        for {
        n <- Gen.choose(0, Int.MaxValue/2-1)
        m <- Gen.choose(0, Int.MaxValue/2-1)
        } yield (n,m)
      )
    Prop.forAll {
      (s: Stream[Int], v: (Int,Int)) => {
        val (n, m) = v
        s.drop(n).drop(m) == s.drop(n+m) } }
  }

  it should "drop the same elements from a List" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def intGen = Arbitrary[Int] (Gen.choose(0,200))

    ("drop from stream" |: 
      Prop.forAll { (s: Stream[Int], n: Int) =>
        s.drop (n).toList == s.toList.drop (n) } )
  }

  it should "lol" in check {
    implicit def arbitraryIntList = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def intGen = Arbitrary[Int] (Gen.choose(0,100))

  Prop.forAll {
    (s: Stream[Int], n: Int) => 
      (n < lengthOfStream (s)) ==> (lengthOfStream (s.drop (n)) == lengthOfStream (s)-n) }
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

  // Helper methods
  def sumOfStream(s: Stream[Int]) = s.foldRight (0)(_+_)

  def lengthOfStream(s: Stream[Int]) = s.foldRight (0)((_,s) => s+1)

  def tailOfStream[A] (s: Stream[A]) = s match {
    case Cons(h,t) => t
    case _ => Empty
  }
}