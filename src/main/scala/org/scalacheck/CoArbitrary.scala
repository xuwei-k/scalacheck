package org.scalacheck

sealed abstract class CoArbitrary[T] {
  def coarbitrary[U](t: T) : Gen[U] => Gen[U]
}

object CoArbitrary {

  import Gen._

  implicit lazy val coarbUnit: CoArbitrary[Unit] = new CoArbitrary[Unit] {
    def coarbitrary[U](t: Unit) = identity[Gen[U]]
  }

  implicit lazy val coarbBool: CoArbitrary[Boolean] = new CoArbitrary[Boolean] {
    def coarbitrary[U](b: Boolean) = if(b) variant(0) else variant(1)
  }

  implicit lazy val coarbInt: CoArbitrary[Int] = new CoArbitrary[Int] {
    def coarbitrary[U](n: Int) = variant(n)
  }

  implicit def coarbOption[T](implicit c: CoArbitrary[T]): CoArbitrary[Option[T]] =
    new CoArbitrary[Option[T]] {
      def coarbitrary[U](o: Option[T]) = o match {
        case None => variant(0)
        case Some(v) => c.coarbitrary(v) compose (variant(1))
      }
    }

  implicit def coarbEither[T, U](implicit ct: CoArbitrary[T], cu: CoArbitrary[U]): CoArbitrary[Either[T, U]] =
    new CoArbitrary[Either[T, U]] {
      def coarbitrary[V](e: Either[T, U]) = e match {
        case Left(l) => ct.coarbitrary(l) compose (variant(0))
        case Right(r) => cu.coarbitrary(r) compose (variant(1))
      }
    }
}
