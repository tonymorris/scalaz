package scalaz

sealed trait \&/[A, B] {
  import \&/._

  def isThis: Boolean =
    this match {
      case This(_) => true
      case That(_) => false
      case Both(_, _) => false
    }

  def isThat: Boolean =
    this match {
      case This(_) => false
      case That(_) => true
      case Both(_, _) => false
    }

  def isBoth: Boolean =
    this match {
      case This(_) => false
      case That(_) => false
      case Both(_, _) => true
    }

  def a: Option[A] =
    this match {
      case This(a) => Some(a)
      case That(_) => None
      case Both(a, _) => Some(a)
    }

  def b: Option[B] =
    this match {
      case This(_) => None
      case That(b) => Some(b)
      case Both(_, b) => Some(b)
    }

  def fold[X](s: A => X, t: B => X, q: (A, B) => X): X =
    this match {
      case This(a) => s(a)
      case That(b) => t(b)
      case Both(a, b) => q(a, b)
    }

  def swap: (B \&/ A) =
    this match {
      case This(a) => That(a)
      case That(b) => This(b)
      case Both(a, b) => Both(b, a)
    }

  def unary_~ : (B \&/ A) =
    swap

  def swapped[AA, BB](k: (B \&/ A) => (BB \&/ AA)): (AA \&/ BB) =
    k(swap).swap

  def ~[AA, BB](k: (B \&/ A) => (BB \&/ AA)): (AA \&/ BB) =
    swapped(k)

  def bimap[C, D](f: A => C, g: B => D): (C \&/ D) =
    this match {
      case This(a) => This(f(a))
      case That(b) => That(g(b))
      case Both(a, b) => Both(f(a), g(b))
    }

  def leftMap[C](f: A => C): (C \&/ B) =
    bimap(f, identity)

  def bitraverse[F[_]: Apply, C, D](f: A => F[C], g: B => F[D]): F[C \&/ D] =
    this match {
      case This(a) =>
        Functor[F].map(f(a))(This(_))
      case That(b) =>
        Functor[F].map(g(b))(That(_))
      case Both(a, b) =>
        Apply[F].apply2(f(a), g(b)) {
          case (c, d) => Both(c, d): C \&/ D
        }
    }

  def map[D](g: B => D): (A \&/ D) =
    bimap(identity, g)

  def traverse[F[_]: Applicative, D](g: B => F[D]): F[A \&/ D] =
    this match {
      case This(a) =>
        Applicative[F].point(This(a))
      case That(b) =>
        Functor[F].map(g(b))(That(_))
      case Both(a, b) =>
        Functor[F].map(g(b))(Both(a, _): A \&/ D)
    }

  def foreach(g: B => Unit): Unit =
    bimap(_ => (), g)

  def flatMap[D](g: B => (A \&/ D))(implicit M: Semigroup[A]): (A \&/ D) =
    this match {
      case This(a) =>
        This(a)
      case That(b) =>
        g(b)
      case Both(a, b) =>
        g(b) match {
          case This(aa) =>
            This(M.append(a, aa))
          case That(bb) =>
            Both(a, bb)
          case Both(aa, bb) =>
            Both(M.append(a, aa), bb)
        }
    }

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z): Z =
    b match {
      case None => z
      case Some(bb) => f(bb, z)
    }

  def exists(p: B => Boolean): Boolean =
    b exists p

  def forall(p: B => Boolean): Boolean =
    b forall p

  def toList: List[B] =
    b.toList

  def getOrElse(bb: => B): B =
    b getOrElse bb

  def |(bb: => B): B =
    getOrElse(bb)

  def valueOr(x: A => B)(implicit M: Semigroup[B]): B =
    this match {
      case This(a) =>
        x(a)
      case That(b) =>
        b
      case Both(a, b) =>
        M.append(x(a), b)
    }

  def ===(x: A \&/ B)(implicit EA: Equal[A], EB: Equal[B]): Boolean =
    this match {
      case This(a) =>
        x match {
          case This(aa) =>
            EA.equal(a, aa)
          case _ =>
            false
        }
      case That(b) =>
        x match {
          case That(bb) =>
            EB.equal(b, bb)
          case _ =>
            false
        }
      case Both(a, b) =>
        x match {
          case Both(aa, bb) =>
            EA.equal(a, aa) && EB.equal(b, bb)
          case _ =>
            false
        }
    }

  def show(implicit SA: Show[A], SB: Show[B]): Cord =
    this match {
      case This(a) =>
        "This(" +: SA.show(a) :+ ")"
      case That(b) =>
        "That(" +: SB.show(b) :+ ")"
      case Both(a, b) =>
        ("Both(" +: SA.show(a) :+ ",") ++ SB.show(b) :+ ")"
    }


}

object \&/ {
  case class This[A, B](aa: A) extends (A \&/ B)
  case class That[A, B](bb: B) extends (A \&/ B)
  case class Both[A, B](aa: A, bb: B) extends (A \&/ B)

  def alignStream[A, B](a: EphemeralStream[A], b: EphemeralStream[B]): EphemeralStream[A \&/ B] =
    if(a.isEmpty)
      b.map(That(_))
    else if(b.isEmpty)
      a.map(This(_))
    else
      EphemeralStream.cons(Both(a.head(), b.head()), alignStream(a.tail(), b.tail()))

  def alignList[A, B](a: List[A], b: List[B]): List[A \&/ B] =
    a match {
      case Nil =>
        b.map(That[A, B](_))
      case h::t =>
        b match {
          case Nil =>
            a.map(This[A, B](_))
          case hh::tt =>
            Both(h, hh) :: alignList(t, tt)
        }

    }

  import scalaz.std.list._

  def concatThisList[A, B](x: List[A \&/ B]): List[A] =
    concatThis[List, A, B](x)

  def concatThisStream[A, B](x: EphemeralStream[A \&/ B]): EphemeralStream[A] =
    concatThis[EphemeralStream, A, B](x)

  def concatThis[F[_], A, B](x: F[A \&/ B])(implicit M: MonadPlus[F]): F[A] =
    M.bind(x) {
      case This(a) =>
        M.point(a)
      case That(_) =>
        M.empty
      case Both(a, _) =>
        M.point(a)
    }

  def concatThatList[A, B](x: List[A \&/ B]): List[B] =
    concatThat[List, A, B](x)

  def concatThatStream[A, B](x: EphemeralStream[A \&/ B]): EphemeralStream[B] =
    concatThat[EphemeralStream, A, B](x)

  def concatThat[F[_], A, B](x: F[A \&/ B])(implicit M: MonadPlus[F]): F[B] =
    M.bind(x) {
      case This(_) =>
        M.empty
      case That(b) =>
        M.point(b)
      case Both(_, b) =>
        M.point(b)
    }

  def unalignList[A, B](x: List[A \&/ B]): (List[A], List[B]) =
    unalign[List, A, B](x)

  def unalignStream[A, B](x: EphemeralStream[A \&/ B]): (EphemeralStream[A], EphemeralStream[B]) =
    unalign[EphemeralStream, A, B](x)

  def unalign[F[_], A, B](x: F[A \&/ B])(implicit M: MonadPlus[F]): (F[A], F[B]) =
    (concatThis(x), concatThat(x))
}
