package scalaz

trait Searchable[A] {
  def search[B](run: A => Maybe[B]): Maybe[B]

  def map[C](f: A => C): Searchable[C] =
    new Searchable[C] {
      def search[B](run: C => Maybe[B]) =
        Searchable.this.search(run compose f)
    }

  def flatMap[C](f: A => Searchable[C]): Searchable[C] =
    new Searchable[C] {
      def search[B](run: C => Maybe[B]) =
        Searchable.this.search(f(_).search(run))
    }

  def exists(p: A => Boolean): Boolean = 
    search(a => if(p(a)) Maybe.just(()) else Maybe.empty[Unit]).isJust
  
  def forall(p: A => Boolean): Boolean =
    !exists(!p(_))

  def maybe: Searchable[Maybe[A]] =
    new Searchable[Maybe[A]] {
      def search[B](run: Maybe[A] => Maybe[B]) =
        run(Maybe.empty) orElse Searchable.this.search(a => run(Maybe.just(a)))
        // search (mamb . Just)
    }
}

object Searchable {
  def unsearch[A](m: Maybe[A]): Searchable[A] =
    new Searchable[A] {
      def search[B](run: A => Maybe[B]) =
        m flatMap run
    }
}
