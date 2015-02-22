package org.scalacheck

abstract class CoArbitrary[T] { self =>
  def coarbitrary[U](t: T) : Gen[U] => Gen[U]

  def contramap[A](f: A => T): CoArbitrary[A] =
    new CoArbitrary[A] {
      def coarbitrary[U](t: A): Gen[U] => Gen[U] =
        self.coarbitrary(f(t))
    }
}

object CoArbitrary {
  def apply[A](implicit A: CoArbitrary[A]): CoArbitrary[A] = A

  import Gen._

  implicit val coarbChar: CoArbitrary[Char] =
    new CoArbitrary[Char] {
      def coarbitrary[U](t: Char) =
        Gen.variant(t << 1)
    }

  implicit def coarbF1[A, B](implicit A: Arbitrary[A], B: CoArbitrary[B]): CoArbitrary[A => B] =
    new CoArbitrary[A => B] {
      def coarbitrary[U](t: A => B) = g =>
        A.arbitrary.flatMap(a => B.coarbitrary(t(a))(g))
    }

  implicit def coarbF2[A1: Arbitrary, A2: Arbitrary, B: CoArbitrary]: CoArbitrary[(A1, A2) => B] =
    CoArbitrary[Tuple2[A1, A2] => B].contramap(_.tupled)

  implicit def coarbF3[A1: Arbitrary, A2: Arbitrary, A3: Arbitrary, B: CoArbitrary]: CoArbitrary[(A1, A2, A3) => B] =
    CoArbitrary[Tuple3[A1, A2, A3] => B].contramap(_.tupled)

  implicit def coarbF4[A1: Arbitrary, A2: Arbitrary, A3: Arbitrary, A4: Arbitrary, B: CoArbitrary]: CoArbitrary[(A1, A2, A3, A4) => B] =
    CoArbitrary[Tuple4[A1, A2, A3, A4] => B].contramap(_.tupled)

  implicit def coarbF5[A1: Arbitrary, A2: Arbitrary, A3: Arbitrary, A4: Arbitrary, A5: Arbitrary, B: CoArbitrary]: CoArbitrary[(A1, A2, A3, A4, A5) => B] =
    CoArbitrary[Tuple5[A1, A2, A3, A4, A5] => B].contramap(_.tupled)

  implicit val coarbString: CoArbitrary[String] =
    CoArbitrary[List[Char]].contramap(_.toList)

  implicit val coarbUnit: CoArbitrary[Unit] = new CoArbitrary[Unit] {
    def coarbitrary[U](t: Unit) = identity[Gen[U]]
  }

  implicit val coarbBool: CoArbitrary[Boolean] = new CoArbitrary[Boolean] {
    def coarbitrary[U](b: Boolean) = if(b) variant(0) else variant(1)
  }

  implicit val coarbInt: CoArbitrary[Int] = new CoArbitrary[Int] {
    def coarbitrary[U](n: Int) = variant(n)
  }

  implicit def coarbOption[T](implicit c: CoArbitrary[T]): CoArbitrary[Option[T]] =
    new CoArbitrary[Option[T]] {
      def coarbitrary[U](o: Option[T]) = o match {
        case None => variant(0)
        case Some(v) => c.coarbitrary(v) compose variant(1)
      }
    }

  implicit def coarbEither[T, U](implicit ct: CoArbitrary[T], cu: CoArbitrary[U]): CoArbitrary[Either[T, U]] =
    new CoArbitrary[Either[T, U]] {
      def coarbitrary[V](e: Either[T, U]) = e match {
        case Left(l) => ct.coarbitrary(l) compose variant(0)
        case Right(r) => cu.coarbitrary(r) compose variant(1)
      }
    }

  implicit def coarbList[A](implicit A: CoArbitrary[A]): CoArbitrary[List[A]] =
    new CoArbitrary[List[A]] {
      def coarbitrary[U](t: List[A]) = g => t match{
        case h :: t =>
          // TODO avoid stack overflow ?
          variant(1)(A.coarbitrary(h)(coarbitrary(t)(g)))
        case Nil =>
          variant(0)(g)
      }
  }

  implicit def coarbStream[A: CoArbitrary]: CoArbitrary[Stream[A]] =
    coarbList[A].contramap(_.toList)

  implicit def coarbVector[A: CoArbitrary]: CoArbitrary[Vector[A]] =
    coarbList[A].contramap(_.toList)

  implicit def coarbTuple1[A: CoArbitrary]: CoArbitrary[Tuple1[A]] =
    CoArbitrary[A].contramap(_._1)

  implicit def coarbTuple2[A1, A2](implicit
                                   A1: CoArbitrary[A1],
                                   A2: CoArbitrary[A2]): CoArbitrary[(A1, A2)] =
    new CoArbitrary[(A1, A2)] {
      def coarbitrary[U](t: (A1, A2)) = g =>
        A1.coarbitrary(t._1)(A2.coarbitrary(t._2)(g))
    }

  implicit def coarbTuple3[A1, A2, A3](implicit
                                       A1: CoArbitrary[A1],
                                       A2: CoArbitrary[A2],
                                       A3: CoArbitrary[A3]): CoArbitrary[(A1, A2, A3)] =
    new CoArbitrary[(A1, A2, A3)] {
      def coarbitrary[U](t: (A1, A2, A3)) = g =>
        A1.coarbitrary(t._1)(A2.coarbitrary(t._2)(A3.coarbitrary(t._3)(g)))
    }

  implicit def coarbTuple4[A1, A2, A3, A4](implicit
                                           A1: CoArbitrary[A1],
                                           A2: CoArbitrary[A2],
                                           A3: CoArbitrary[A3],
                                           A4: CoArbitrary[A4]): CoArbitrary[(A1, A2, A3, A4)] =
    new CoArbitrary[(A1, A2, A3, A4)] {
      def coarbitrary[U](t: (A1, A2, A3, A4)) = g =>
        A1.coarbitrary(t._1)(A2.coarbitrary(t._2)(A3.coarbitrary(t._3)(A4.coarbitrary(t._4)(g))))
    }

  implicit def coarbTuple5[A1, A2, A3, A4, A5](implicit
                                           A1: CoArbitrary[A1],
                                           A2: CoArbitrary[A2],
                                           A3: CoArbitrary[A3],
                                           A4: CoArbitrary[A4],
                                           A5: CoArbitrary[A5]): CoArbitrary[(A1, A2, A3, A4, A5)] =
    new CoArbitrary[(A1, A2, A3, A4, A5)] {
      def coarbitrary[U](t: (A1, A2, A3, A4, A5)) = g =>
        A1.coarbitrary(t._1)(A2.coarbitrary(t._2)(A3.coarbitrary(t._3)(A4.coarbitrary(t._4)(A5.coarbitrary(t._5)(g)))))
    }
}
