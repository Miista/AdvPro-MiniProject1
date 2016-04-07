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

  it should "not force the tail" in {
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
      (n > s.length()) ==> (s.take(n) === s)
    }
  }

  it should "return a subset of the Stream" in check {
    Prop.forAll (streams) { (s) => {
      val n = s.length()-1
      (n > 0) ==> (!(s === s.take (n))) } }
  }

  it should "respect transitivity" in check {
    Prop.forAll(streams, Gen.choose(1, 10)) { (s, n) => {
      val m = n-1
      (n < s.length()) ==> (s.take(n).take(m) === s.take(m))
    } }
  }

  it should "respect commutativity" in check {
    val ints = Gen.choose(1, 10)
    Prop.forAll(streams, ints, ints) { (s, n, m) => {
      s.take(n).take(m) === s.take(m).take(n)
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
        s1.append (s2).length() == s1.length() + s2.length()
      }
    ) && 
    ("test2" |:
      Prop.forAll(streams, streams, streams) { (s1,s2,s3) =>
        s1.length() + s2.append (s3).length() == s1.append(s2).length() + s3.length()
      }
    )
  }

  it should "respect identity" in {
    val stream: Stream[Int] = streams.sample.get
    assert (stream.append (empty) === stream)
  }

  // MAP
  behavior of "map"

  it should "respect identity" in check {
    Prop.forAll(streams) { (s) =>
      s.map (identity) === s
    }
  }

  it should "respect associativity" in check {
    def f(n: Int): Int = n+1
    def g(n: Int): Int = n*2
    Prop.forAll(streams, positiveInts) { (s, n) => 
      s.map(f).map(g) === s.map(f _ andThen g) }
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
      s.drop(0) === s
    }
  }

  it should "return empty when n > |S|" in check {
    Prop.forAll(streams, positiveInts) { (s,n) =>
      (n > s.length()) ==> (s.drop(n) == empty)
    }
  }

  it should "respect additivity" in check {
    Prop.forAll(streams, Gen.choose(0,20), Gen.choose(0,20)) { (s,m,n) =>
      (m+n < s.length()) ==> (s.drop(m).drop(n) === s.drop(m+n))
    }
  }

  it should "respect commutativity" in check {
    val ints = Gen.choose(1, 10)
    Prop.forAll(streams, ints, ints) { (s, n, m) => {
      s.drop(n).drop(m) === s.drop(m).drop(n)
    } }
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


  implicit class EqStream[A] (s: Stream[A]) {
    private def isEqual[Int] (s1: Stream[Int], s2: Stream[Int]) = {
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

    def ===(that: Stream[A]) = isEqual(s, that)

    def ≡(that: Stream[A]) = isEqual(s, that)

    def length() = s.foldRight (0)((_,s) => s+1)
  }
}