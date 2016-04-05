package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import stream00._

class StreamSpecSpalInau extends FlatSpec with Checkers {
  import Stream._

  behavior of "headOption"

  it should "not force the" in {
    val s = cons(1, cons(throw new java.lang.Exception(), cons(println("hey"), empty)))
    s.headOption
  }

  // TAKE
  behavior of "take"

  it should "return an empty Stream when taking from empty" in {
    assert (empty.take(5) == empty)
  }

  it should "return the entire Stream" in check {
    Prop.forAll (streams, positiveInts) { (s, n) =>
      (n > lengthOfStream(s)) ==> (isEqual (s.take(n), s))
    }
  }

  it should "return a subset of the Stream" in check {
    Prop.forAll (streams) { (s) => {
      val n = lengthOfStream (s)-1
      (n > 0) ==> (!isEqual(s, s.take (n))) } }
  }

  it should "respect transitivity" in check {
    Prop.forAll(streams, Gen.choose(1, 10)) { (s, n) => {
      val m = n-1
      (n < lengthOfStream(s)) ==> (isEqual(s.take(n).take(m), s.take(m)))
    } }
  }

  it should "return an empty Stream when taking zero" in {
    assert (streams.sample.get.take(0) == empty)
  }

  // APPEND
  behavior of "append"

  it should "respect associativity" in check {
    ("test 1" |:
      Prop.forAll(streams, streams) { (s1,s2) =>
        lengthOfStream(
          s1.append (s2)) == lengthOfStream(s1) + lengthOfStream(s2)
      }
    ) && 
    ("test2" |:
      Prop.forAll(streams, streams, streams) { (s1,s2,s3) =>
        lengthOfStream(s1) + lengthOfStream(
          s2.append (s3)) == lengthOfStream(
            s1.append(s2)) + lengthOfStream(s3)
      }
    )
  }

  it should "respect identity" in {
    val stream: Stream[Int] = streams.sample.get
    assert (isEqual (stream.append (empty), stream))
  }

  // MAP
  behavior of "map"

  it should "respect identity" in check {
    Prop.forAll(streams) { (s) =>
      isEqual (s.map (identity), s)
    }
  }

  it should "respect associativity" in check {
    def f(n: Int): Int = n+1
    def g(n: Int): Int = n*2
    Prop.forAll(streams, positiveInts) { (s, n) => 
      isEqual (s.map(f).map(g), s.map(f _ andThen g)) }
  }

  // DROP
  behavior of "drop"

  it should "return an empty Stream" in check {
    Prop.forAll(arbitrary[Int]) { (n) =>
      empty.drop (n) == empty
    }
  }

  it should "respect identity" in check {
    Prop.forAll(streams) { (s) =>
      isEqual (s.drop(0), s)
    }
  }

  it should "return empty when n > |S|" in check {
    Prop.forAll(streams, positiveInts) { (s,n) =>
      (n > lengthOfStream(s)) ==> (s.drop(n) == empty)
    }
  }

  it should "respect associativity" in check {
    Prop.forAll(streams, Gen.choose(0,20), Gen.choose(0,20)) { (s,m,n) =>
      (m+n < lengthOfStream(s)) ==> (isEqual (s.drop(m).drop(n), s.drop(m+n)))
    }
  }

  val streams = genNonEmptyStream[Int]
  val positiveInts = arbitrary[Int].suchThat (0 < _)
  
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
  def lengthOfStream(s: Stream[Int]) = s.foldRight (0)((_,s) => s+1)

  def isEqual[Int] (s1: Stream[Int], s2: Stream[Int]) = {
    val bools: Stream[Boolean] = s1.zipWithAll[Int, Boolean] (s2)(
      (o1: Option[Int],o2: Option[Int]) => {
        val cond: Option[Boolean] = o1.flatMap((a: Int) => o2.map ((b: Int) => a == b));
        cond match {
          case None => false
          case Some(x) => x
        }
      }
    )
    bools.forAll (_ == true)
  }

  // trait Eq[A] extends Stream[A] {
  //   def == (that: Stream[A]): Boolean = {
  //     def isEqual[Int] (s1: Stream[Int], s2: Stream[Int]) = {
  //       val bools: Stream[Boolean] = s1.zipWithAll[Int, Boolean] (s2)(
  //         (o1: Option[Int],o2: Option[Int]) => {
  //           val cond: Option[Boolean] = o1.flatMap((a: Int) => o2.map ((b: Int) => a == b));
  //           cond match {
  //             case None => false
  //             case Some(x) => x
  //           }
  //         }
  //       )
  //       bools.forAll (_ == true)
  //     }
  //     isEqual (this, that)
  //   }
  // }
}