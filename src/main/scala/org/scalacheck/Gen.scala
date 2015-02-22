/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import language.higherKinds
import language.implicitConversions

import util.Buildable
import scala.collection.immutable.TreeMap

sealed trait Gen[+T] {

  //// Private interface ////

  import Gen._

  /** Just an alias */
  private type P = Gen.Parameters

  //// Public interface ////

  /** Evaluate this generator with the given parameters */
  def apply(p: Gen.Parameters): T

  /** Create a new generator by mapping the result of this generator */
  def map[U](f: T => U): Gen[U] = gen { p => f(this(p)) }

  /** Create a new generator by flat-mapping the result of this generator */
  def flatMap[U](f: T => Gen[U]): Gen[U] = gen { p =>
    f(this(p))(p)
  }

  def withFilter(p: T => Boolean): Gen[T] = suchThat(p)

  def filter(p: T => Boolean): Gen[T] = suchThat(p)

  /** Create a new generator that uses this generator to produce a value
   *  that fulfills the given condition. If the condition is not fulfilled,
   *  the generator fails (returns None). This method is identical to
   *  [Gen.filter]. */
  def suchThat(f: T => Boolean): Gen[T] = gen { p =>
    var g = suchThatOption(f)
    var r = g(p)
    while(r.isEmpty) {
      g = sized(n => resize(n + 1, suchThatOption(f)))
      r = g(p)
    }
    r.get
  }

  private def suchThatOption(f: T => Boolean): Gen[Option[T]] = {
    def go(k: Int, n: Int): Gen[Option[T]] =
      if(n == 0) None
      else gen { p =>
        var nn = n
        var kk = k
        def rp(i: Int, j: Int, g: Gen[T]): Gen[T] = resize(2 * i + j, g)
        var g = rp(k, n, this)
        var x = g(p)
        while(nn != 0 && !f(x)) {
          nn = nn - 1
          kk = kk + 1
          g = rp(kk, nn, g)
          x = g(p)
        }
        if(nn == 0) None else Some(x)
      }
    sized(n => go(0, math.max(1, n)))
  }

  /** Create a generator that calls this generator repeatedly until
   *  the given condition is fulfilled. The generated value is then
   *  returned. Use this combinator with care, since it may result
   *  in infinite loops. */
  def retryUntil(p: T => Boolean): Gen[T] = flatMap { t =>
    if (p(t)) const(t) else retryUntil(p)
  }

  def sample: Option[T] = Some(this(Gen.Parameters.default))

  /** Returns a new property that holds if and only if both this
   *  and the given generator generates the same result, or both
   *  generators generate no result.  */
  def ==[U](g: Gen[U]) = Prop { prms =>
    if(this(prms) == g(prms)) Prop.proved(prms)
    else Prop.falsified(prms)
  }

  def !=[U](g: Gen[U]) = Prop.forAll(this)(r => Prop.forAll(g)(_ != r))

  def !==[U](g: Gen[U]) = Prop { prms =>
    if(this(prms) == g(prms)) Prop.falsified(prms)
    else Prop.proved(prms)
  }

}

object Gen {

  //// Private interface ////

  import Arbitrary.arbitrary

  /** Just an alias */
  private type P = Parameters

  /** Generator factory method */
  private[scalacheck] def gen[T](f: P => T): Gen[T] = new Gen[T] {
    def apply(p: P) = f(p)
  }

  //// Public interface ////

  /** Generator parameters, used by [[org.scalacheck.Gen.apply]] */
  trait Parameters {

    /** The size of the generated value. Generator implementations are allowed
     *  to freely interpret (or ignore) this value. During test execution, the
     *  value of this parameter is controlled by [[Test.Parameters.minSize]] and
     *  [[Test.Parameters.maxSize]]. */
    val size: Int

    /** Create a copy of this [[Gen.Parameters]] instance with
     *  [[Gen.Parameters.size]] set to the specified value. */
    def withSize(size: Int): Parameters = cp(size = size)

    /** The random number generator used. */
    val rng: scala.util.Random

    /** Create a copy of this [[Gen.Parameters]] instance with
     *  [[Gen.Parameters.rng]] set to the specified value. */
    def withRng(rng: scala.util.Random): Parameters = cp(rng = rng)

    // private since we can't guarantee binary compatibility for this one
    private case class cp(
      size: Int = size,
      rng: scala.util.Random = rng
    ) extends Parameters
  }

  /** Provides methods for creating [[org.scalacheck.Gen.Parameters]] values */
  object Parameters {
    /** Default generator parameters trait. This can be overriden if you
     *  need to tweak the parameters. */
    trait Default extends Parameters {
      val size: Int = 100
      val rng: scala.util.Random = scala.util.Random
    }

    /** Default generator parameters instance. */
    val default: Parameters = new Default {}
  }

  /** A wrapper type for range types */
  trait Choose[T] {
    /** Creates a generator that returns a value in the given inclusive range */
    def choose(min: T, max: T): Gen[T]
  }

  /** Provides implicit [[org.scalacheck.Gen.Choose]] instances */
  object Choose {

    private def chLng(l: Long, h: Long)(p: P): Long = {
      val f = math.min(l, h)
      val t = math.max(l, h)
      val d = t - l + 1
      if (d <= 0) {
        var n = p.rng.nextLong
        while (n < f || n > t) {
          n = p.rng.nextLong
        }
        n
      } else {
        f + math.abs(p.rng.nextLong % d)
      }
    }

    private def chDbl(l: Double, h: Double)(p: P): Double = {
      val f = math.min(l, h)
      val t = math.max(l, h)
      val d = t - f
      if (d == 0) f
      else p.rng.nextDouble * (t - f) + f
    }

    implicit val chooseLong: Choose[Long] = new Choose[Long] {
      def choose(low: Long, high: Long) =
        gen(chLng(low,high))
    }
    implicit val chooseInt: Choose[Int] = new Choose[Int] {
      def choose(low: Int, high: Int) =
        gen(chLng(low,high)).map(_.toInt)
    }
    implicit val chooseByte: Choose[Byte] = new Choose[Byte] {
      def choose(low: Byte, high: Byte) =
        gen(chLng(low,high)).map(_.toByte)
    }
    implicit val chooseShort: Choose[Short] = new Choose[Short] {
      def choose(low: Short, high: Short) =
        gen(chLng(low,high)).map(_.toShort)
    }
    implicit val chooseChar: Choose[Char] = new Choose[Char] {
      def choose(low: Char, high: Char) =
        gen(chLng(low,high)).map(_.toChar)
    }
    implicit val chooseDouble: Choose[Double] = new Choose[Double] {
      def choose(low: Double, high: Double) =
        gen(chDbl(low,high))
    }
    implicit val chooseFloat: Choose[Float] = new Choose[Float] {
      def choose(low: Float, high: Float) =
        gen(chDbl(low,high)).map(_.toFloat)
    }

    /** Transform a Choose[T] to a Choose[U] where T and U are two isomorphic
     *  types whose relationship is described by the provided transformation
     *  functions. (exponential functor map) */
    def xmap[T, U](from: T => U, to: U => T)(implicit c: Choose[T]): Choose[U] =
      new Choose[U] {
        def choose(low: U, high: U) =
          c.choose(to(low), to(high)).map(from)
      }
  }


  //// Various Generator Combinators ////

  /** A generator that always generates the given value */
  implicit def const[T](x: T): Gen[T] = gen(_ => x)

  /** A generator that generates a random value in the given (inclusive)
   *  range. If the range is invalid, the generator will not generate
   *  any value. */
  def choose[T](min: T, max: T)(implicit c: Choose[T]): Gen[T] =
    c.choose(min, max)

  /** Sequences generators. If any of the given generators fails, the
   *  resulting generator will also fail. */
  def sequence[C,T](gs: Traversable[Gen[T]])(implicit b: Buildable[T,C]): Gen[C] = {
    val g = gen { p =>
      gs.foldLeft(collection.immutable.Vector.empty[T]) {
        case (rs,g) => rs :+ g(p)
      }
    }
    g.map(b.fromIterable)
  }

  /** Wraps a generator lazily. The given parameter is only evaluated once,
   *  and not until the wrapper generator is evaluated. */
  def lzy[T](g: => Gen[T]): Gen[T] = {
    lazy val h = g
    gen { p => h(p) }
  }

  /** Wraps a generator for later evaluation. The given parameter is
   *  evaluated each time the wrapper generator is evaluated. */
  def wrap[T](g: => Gen[T]) = gen { p => g(p) }

  /** Creates a generator that can access its generation parameters */
  def parameterized[T](f: Parameters => Gen[T]) = gen { p => f(p)(p) }

  /** Creates a generator that can access its generation size */
  def sized[T](f: Int => Gen[T]) = gen { p => f(p.size)(p) }

  /** A generator that returns the current generation size */
  lazy val size: Gen[Int] = sized { sz => sz }

  /** Creates a resized version of a generator */
  def resize[T](s: Int, g: Gen[T]) = gen(p => g(p.withSize(s)))

  /** Picks a random value from a list */
  def oneOf[T](xs: Seq[T]): Gen[T] =
    choose(0, xs.size-1).map(xs(_)).suchThat(xs.contains)

  /** Picks a random value from a list */
  def oneOf[T](t0: T, t1: T, tn: T*): Gen[T] = oneOf(t0 +: t1 +: tn)

  /** Picks a random generator from a list */
  def oneOf[T](g0: Gen[T], g1: Gen[T], gn: Gen[T]*): Gen[T] = {
    val gs = g0 +: g1 +: gn
    choose(0,gs.size-1).flatMap(gs(_))
  }

  /** Makes a generator result optional. Either `Some(T)` or `None` will be provided. */
  def option[T](g: Gen[T]): Gen[Option[T]] =
    oneOf[Option[T]](g.map(Some.apply), None)

  /** Chooses one of the given generators with a weighted random distribution */
  def frequency[T](gs: (Int,Gen[T])*): Gen[T] = {
    gs.filter(_._1 > 0) match {
      case Nil => throw new IllegalArgumentException("Gen.frequency require non-empty list.")
      case filtered =>
        var tot = 0l
        val tree: TreeMap[Long, Gen[T]] = {
          val builder = TreeMap.newBuilder[Long, Gen[T]]
          filtered.foreach {
            case (f, v) =>
              tot += f
              builder.+=((tot, v))
          }
          builder.result()
        }
        choose(1L, tot).flatMap(r => tree.from(r).head._2)
    }
  }

  /** Implicit convenience method for using the `frequency` method
   *  like this:
   *  {{{
   *   frequency((1, "foo"), (3, "bar"))
   *  }}}
   */
  implicit def freqTuple[T](t: (Int,T)): (Int,Gen[T]) = (t._1, const(t._2))


  //// List Generators ////

  /** Generates a container of any Traversable type for which there exists an
   *  implicit [[org.scalacheck.util.Buildable]] instance. The elements in the
   *  container will be generated by the given generator. The size of the
   *  generated container is limited by `n`. Depending on what kind of container
   *  that is generated, the resulting container may contain fewer elements than
   *  `n`, but not more. If the given generator fails generating a value, the
   *  complete container generator will also fail. */
  def buildableOfN[C,T](n: Int, g: Gen[T])(implicit
    evb: Buildable[T,C], evt: C => Traversable[T]
  ): Gen[C] =
    sequence[C,T](Traversable.fill(n)(g))

  /** Generates a container of any Traversable type for which there exists an
   *  implicit [[org.scalacheck.util.Buildable]] instance. The elements in the
   *  container will be generated by the given generator. The size of the
   *  container is bounded by the size parameter used when generating values. */
  def buildableOf[C,T](g: Gen[T])(implicit
    evb: Buildable[T,C], evt: C => Traversable[T]
  ): Gen[C] =
    sized(s => choose(0,s).flatMap(buildableOfN[C,T](_,g)))

  /** Generates a non-empty container of any Traversable type for which there
   *  exists an implicit [[org.scalacheck.util.Buildable]] instance. The
   *  elements in the container will be generated by the given generator. The
   *  size of the container is bounded by the size parameter used when
   *  generating values. */
  def nonEmptyBuildableOf[C,T](g: Gen[T])(implicit
    evb: Buildable[T,C], evt: C => Traversable[T]
  ): Gen[C] =
    sized(s => choose(1,s).flatMap(buildableOfN[C,T](_,g))) suchThat { c =>
      c.size > 0
    }

  /** A convenience method for calling `buildableOfN[C[T],T](n,g)`. */
  def containerOfN[C[_],T](n: Int, g: Gen[T])(implicit
    evb: Buildable[T,C[T]], evt: C[T] => Traversable[T]
  ): Gen[C[T]] = buildableOfN[C[T],T](n,g)

  /** A convenience method for calling `buildableOf[C[T],T](g)`. */
  def containerOf[C[_],T](g: Gen[T])(implicit
    evb: Buildable[T,C[T]], evt: C[T] => Traversable[T]
  ): Gen[C[T]] = buildableOf[C[T],T](g)

  /** A convenience method for calling `nonEmptyBuildableOf[C[T],T](g)`. */
  def nonEmptyContainerOf[C[_],T](g: Gen[T])(implicit
    evb: Buildable[T,C[T]], evt: C[T] => Traversable[T]
  ): Gen[C[T]] = nonEmptyBuildableOf[C[T],T](g)

  /** Generates a list of random length. The maximum length depends on the
   *  size parameter. This method is equal to calling
   *  `containerOf[List,T](g)`. */
  def listOf[T](g: => Gen[T]) = buildableOf[List[T],T](g)

  /** Generates a non-empty list of random length. The maximum length depends
   *  on the size parameter. This method is equal to calling
   *  `nonEmptyContainerOf[List,T](g)`. */
  def nonEmptyListOf[T](g: => Gen[T]) = nonEmptyBuildableOf[List[T],T](g)

  /** Generates a list of the given length. This method is equal to calling
   *  `containerOfN[List,T](n,g)`. */
  def listOfN[T](n: Int, g: Gen[T]) = buildableOfN[List[T],T](n,g)

  /** Generates a map of random length. The maximum length depends on the
   *  size parameter. This method is equal to calling
   *  <code>containerOf[Map,T,U](g)</code>. */
  def mapOf[T,U](g: => Gen[(T,U)]) = buildableOf[Map[T,U],(T,U)](g)

  /** Generates a non-empty map of random length. The maximum length depends
   *  on the size parameter. This method is equal to calling
   *  <code>nonEmptyContainerOf[Map,T,U](g)</code>. */
  def nonEmptyMap[T,U](g: => Gen[(T,U)]) = nonEmptyBuildableOf[Map[T,U],(T,U)](g)

  /** Generates a map of with at least the given number of elements. This method
   *  is equal to calling <code>containerOfN[Map,T,U](n,g)</code>. */
  def mapOfN[T,U](n: Int, g: Gen[(T,U)]) = buildableOfN[Map[T,U],(T,U)](n,g)

  /** A generator that picks a random number of elements from a list */
  def someOf[T](l: Iterable[T]) = choose(0,l.size).flatMap(pick(_,l))

  /** A generator that picks a random number of elements from a list */
  def someOf[T](g1: Gen[T], g2: Gen[T], gs: Gen[T]*) =
    choose(0, gs.length+2).flatMap(pick(_, g1, g2, gs: _*))

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, l: Iterable[T]): Gen[Seq[T]] =
    if(n > l.size || n < 0) throw new IllegalArgumentException("Gen.pick require non-empty list and positive number.")
    else (gen { p =>
      val b = new collection.mutable.ListBuffer[T]
      b ++= l
      while(b.length > n) b.remove(choose(0, b.length-1).apply(p))
      b
    }).suchThat(_.forall(x => l.exists(x == _)))

  /** A generator that picks a given number of elements from a list, randomly */
  def pick[T](n: Int, g1: Gen[T], g2: Gen[T], gn: Gen[T]*): Gen[Seq[T]] = {
    val gs = g1 +: g2 +: gn
    pick(n, 0 until gs.size).flatMap(idxs =>
      sequence[List[T],T](idxs.toList.map(gs(_)))
    )
  }


  //// Character Generators ////

  /** Generates a numerical character */
  def numChar: Gen[Char] = choose(48.toChar, 57.toChar)

  /** Generates an upper-case alpha character */
  def alphaUpperChar: Gen[Char] = choose(65.toChar, 90.toChar)

  /** Generates a lower-case alpha character */
  def alphaLowerChar: Gen[Char] = choose(97.toChar, 122.toChar)

  /** Generates an alpha character */
  def alphaChar = frequency((1,alphaUpperChar), (9,alphaLowerChar))

  /** Generates an alphanumerical character */
  def alphaNumChar = frequency((1,numChar), (9,alphaChar))


  //// String Generators ////

  /** Generates a string that starts with a lower-case alpha character,
   *  and only contains alphanumerical characters */
  def identifier: Gen[String] = (for {
    c <- alphaLowerChar
    cs <- listOf(alphaNumChar)
  } yield (c::cs).mkString).suchThat(_.forall(c => c.isLetter || c.isDigit))

  /** Generates a string of alpha characters */
  def alphaStr: Gen[String] =
    listOf(alphaChar).map(_.mkString).suchThat(_.forall(_.isLetter))

  /** Generates a string of digits */
  def numStr: Gen[String] =
    listOf(numChar).map(_.mkString).suchThat(_.forall(_.isDigit))


  //// Number Generators ////

  /** Generates positive numbers of uniform distribution, with an
   *  upper bound of the generation size parameter. */
  def posNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
    import num._
    sized(max => c.choose(one, fromInt(max)))
  }

  /** Generates negative numbers of uniform distribution, with an
   *  lower bound of the negated generation size parameter. */
  def negNum[T](implicit num: Numeric[T], c: Choose[T]): Gen[T] = {
    import num._
    sized(max => c.choose(-fromInt(max), -one))
  }

  /** Generates numbers within the given inclusive range, with
   *  extra weight on zero, +/- unity, both extremities, and any special
   *  numbers provided. The special numbers must lie within the given range,
   *  otherwise they won't be included. */
  def chooseNum[T](minT: T, maxT: T, specials: T*)(
    implicit num: Numeric[T], c: Choose[T]
  ): Gen[T] = {
    import num._
    val basics = List(minT, maxT, zero, one, -one)
    val basicsAndSpecials = for {
      t <- specials ++ basics if t >= minT && t <= maxT
    } yield (1, const(t))
    val allGens = basicsAndSpecials ++ List(
      (basicsAndSpecials.length, c.choose(minT, maxT))
    )
    frequency(allGens: _*)
  }

  /** Generates a version 4 (random) UUID. */
  lazy val uuid: Gen[java.util.UUID] = for {
    l1 <- Gen.choose(Long.MinValue, Long.MaxValue)
    l2 <- Gen.choose(Long.MinValue, Long.MaxValue)
    y <- Gen.oneOf('8', '9', 'a', 'b')
  } yield java.util.UUID.fromString(
    new java.util.UUID(l1,l2).toString.updated(14, '4').updated(19, y)
  )

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2](g1: Gen[T1], g2: Gen[T2]): Gen[(T1,T2)] = for {
    t1 <- g1; t2 <- g2
  } yield (t1,t2)

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3]): Gen[(T1,T2,T3)] = {
    val g0 = zip(g1, g2)
    for {
      (t1,t2) <- g0; t3 <- g3
    } yield (t1,t2,t3)
  }

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3,T4](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4]
  ): Gen[(T1,T2,T3,T4)] = {
    val g0 = zip(g1, g2, g3)
    for {
      (t1,t2,t3) <- g0; t4 <- g4
    } yield (t1,t2,t3,t4)
  }

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3,T4,T5](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4],
    g5: Gen[T5]
  ): Gen[(T1,T2,T3,T4,T5)] = {
    val g0 = zip(g1,g2,g3,g4)
    for {
      (t1,t2,t3,t4) <- g0; t5 <- g5
    } yield (t1,t2,t3,t4,t5)
  }

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3,T4,T5,T6](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4],
    g5: Gen[T5], g6: Gen[T6]
  ): Gen[(T1,T2,T3,T4,T5,T6)] = {
    val g0 = zip(g1,g2,g3,g4,g5)
    for {
      (t1,t2,t3,t4,t5) <- g0; t6 <- g6
    } yield (t1,t2,t3,t4,t5,t6)
  }

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3,T4,T5,T6,T7](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3],
    g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7]
  ): Gen[(T1,T2,T3,T4,T5,T6,T7)] = {
    val g0 = zip(g1,g2,g3,g4,g5,g6)
    for {
      (t1,t2,t3,t4,t5,t6) <- g0; t7 <- g7
    } yield (t1,t2,t3,t4,t5,t6,t7)
  }

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3,T4,T5,T6,T7,T8](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3],
    g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8]
  ): Gen[(T1,T2,T3,T4,T5,T6,T7,T8)] = {
    val g0 = zip(g1,g2,g3,g4,g5,g6,g7)
    for {
      (t1,t2,t3,t4,t5,t6,t7) <- g0; t8 <- g8
    } yield (t1,t2,t3,t4,t5,t6,t7,t8)
  }

  /** Combines the given generators into one generator that produces a
   *  tuple of their generated values. */
  def zip[T1,T2,T3,T4,T5,T6,T7,T8,T9](g1: Gen[T1], g2: Gen[T2], g3: Gen[T3],
    g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8], g9: Gen[T9]
  ): Gen[(T1,T2,T3,T4,T5,T6,T7,T8,T9)] = {
    val g0 = zip(g1,g2,g3,g4,g5,g6,g7,g8)
    for {
      (t1,t2,t3,t4,t5,t6,t7,t8) <- g0; t9 <- g9
    } yield (t1,t2,t3,t4,t5,t6,t7,t8,t9)
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T,R](f: T => R)(implicit a: Arbitrary[T]): Gen[R] =
    arbitrary[T] map f

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,R](f: (T1,T2) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2]
  ): Gen[R] = arbitrary[T1] flatMap { t => resultOf(f(t, _:T2)) }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,R](f: (T1,T2,T3) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3]
  ): Gen[R] = arbitrary[T1] flatMap { t => resultOf(f(t, _:T2, _:T3)) }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,R](f: (T1,T2,T3,T4) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,R](f: (T1,T2,T3,T4,T5) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,R](
    f: (T1,T2,T3,T4,T5,T6) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3],
    a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,T7,R](
    f: (T1,T2,T3,T4,T5,T6,T7) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3],
    a4: Arbitrary[T4], a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,T7,T8,R](
    f: (T1,T2,T3,T4,T5,T6,T7,T8) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7, _:T8))
  }

  /** Takes a function and returns a generator that generates arbitrary
   *  results of that function by feeding it with arbitrarily generated input
   *  parameters. */
  def resultOf[T1,T2,T3,T4,T5,T6,T7,T8,T9,R](
    f: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => R)(implicit
    a1: Arbitrary[T1], a2: Arbitrary[T2], a3: Arbitrary[T3], a4: Arbitrary[T4],
    a5: Arbitrary[T5], a6: Arbitrary[T6], a7: Arbitrary[T7], a8: Arbitrary[T8],
    a9: Arbitrary[T9]
  ): Gen[R] = arbitrary[T1] flatMap {
    t => resultOf(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7, _:T8, _:T9))
  }

  private[scalacheck] object Random {

    def boolVariant(b: Boolean, r: scala.util.Random): scala.util.Random = {
      val x = r.nextLong()
      val y = r.nextLong()
      new scala.util.Random(if(b) y else x)
    }

    def chop(n: Int): Int = n % 2

    def even(n: Int): Boolean = n % 2 == 0

    def chip(finished: Boolean, n: Int, s: scala.util.Random): scala.util.Random = boolVariant(finished, boolVariant(even(n), s))

    def stop(n: Int): Boolean = n <= 1

    def bigNatVariant(n: Int, r: scala.util.Random): scala.util.Random =
      if(stop(n)) chip(true, n, r)
      else bigNatVariant(chop(n), chip(false, n, r))

    def natVariant(n: Int, r: scala.util.Random): scala.util.Random =
      if(stop(n)) chip(true, n, r)
      else bigNatVariant(n, r)

    def variantRandom(n: Int, r: scala.util.Random): scala.util.Random =
      if (n >= 1) natVariant(n - 1, boolVariant(false, r))
      else if(n == 0) natVariant(0, boolVariant(true, r))
      else bigNatVariant(-n, boolVariant(true, r))
  }

  def variant[T](n: Int): Gen[T] => Gen[T] = g => gen { p =>
    g(p.withRng(Random.variantRandom(n, p.rng)))
  }

  def promote[T, U](f: T => Gen[U]): Gen[T => U] =
    gen(p => x => f(x)(p))
}
