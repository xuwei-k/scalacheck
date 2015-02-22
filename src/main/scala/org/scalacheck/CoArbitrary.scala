package org.scalacheck

abstract class CoArbitrary[T] { self =>
  def coarbitrary[U](t: T) : Gen[U] => Gen[U]

  def contramap[A](f: A => T): CoArbitrary[A] =
    new CoArbitrary[A] {
      def coarbitrary[U](t: A): (Gen[U]) => Gen[U] =
        self.coarbitrary(f(t))
    }
}

object CoArbitrary {
  import Gen._

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

  implicit def coarbVector[A: CoArbitrary]: CoArbitrary[Vector[A]] =
    coarbList[A].contramap(_.toList)

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
