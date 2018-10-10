// Applicative typeclass
trait Applicative[F[_]] {
  def map[A, B](a: F[A], f: A => B): F[B]
  def pure[A](a: A): F[A]
  def ap[A, B](f: F[A => B], a: F[A]): F[B]
  def map2[A, B, C](a: F[A], b: F[B], f: (A, B) => C): F[C] = ap(ap(pure((x: A) => (y: B) => f(x, y)), a), b)
}

trait Monoid[F] {
  def combine(a: F, b: F): F
  def pure: F
  def fold(l: TraversableOnce[F]): F = l.fold(pure)((p, c) => combine(p,c))
}

object MonoidInstances {
  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def combine(a: List[A], b : List[A]): List[A] = a ++ b
    def pure = List.empty[A]
  }
}

object MonoidSyntax {
  implicit class MonoidWrapper[F](m: F)(implicit ev: Monoid[F]) {
    def <>(other: F): F = ev.combine(m, other)
  }
}
object Applicative {
  def apply[F[_] : Applicative] : Applicative[F] = implicitly
}

case class Const[M, A](val m: M) {
  def covary[B]: Const[M, B] = this.asInstanceOf[Const[M, B]]
}

object ApplicativeInstances {
  import MonoidSyntax._

  implicit def constApplicative[M](implicit ev : Monoid[M]): Applicative[[B] => Const[M, B]] = new Applicative[[B] => Const[M, B]] {
    def pure[A](a: A): Const[M, A]= Const[M, A](ev.pure)
    def ap[A, B](f: Const[M, A => B], a: Const[M, A]): Const[M, B] = {
      Const(a.m <> f.m)
    }
    def map[A, B](a: Const[M, A], f: A => B): Const[M, B] = a.covary[B]
  }
}

object Monad {
  def apply[F[_] : Monad]: Monad[F] = implicitly
}
trait Monad[F[_]] extends Applicative[F]{
  def pure[A](a: A): F[A]
  def flatMap[A, B](a: F[A], f: A => F[B]): F[B]

  def map[A, B](a: F[A], f: A => B): F[B] = flatMap(a, ap => pure(f(ap)))
  def ap[A, B](f: F[A => B], a: F[A]): F[B] = flatMap(f, fp => map(a, ap => fp(ap)))
}

object MonadSyntax {
  implicit class MonadWrapper[T[_], A](val c: T[A])(implicit ec: Monad[T]) {
    def flatMap[B](f: A => T[B]): T[B] = ec.flatMap(c, f)
    def map[B](f: A => B): T[B] = ec.map(c, f)
  }
}

object State {
  def runState[S, A](s: State[S, A], initialState: S): (S, A) = s.f(initialState)
  def execState[S, A](s: State[S, A], initialState: S): S = s.f(initialState)._1
  def setState[S](s: S): State[S, Unit] = State(cs => (s, ()))
  def getState[S]: State[S, S] = State(cs => (cs, cs))
  def gets[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
}
// State monad
case class State[S, A](f: S => (S, A))

object MonadInstances {
  implicit def stateMonadS[S]: Monad[[A] => State[S, A]] = new Monad[[A] => State[S, A]] {
    def flatMap[A, B](ma: State[S, A], f: A => State[S, B]): State[S, B] = State[S, B](cs => {
      val (is, a) = State.runState(ma, cs)
      State.runState(f(a),is)
    })
    def pure[A](a: A): State[S, A] = State[S, A](s => (s, a))
  }
}

object Tasks {
  // contract for a store
  trait Store[I, K, V] {
    def getInfo: I
    def putInfo(i: I): Store[I, K, V]
    def putValue(k: K, v: V): Store[I, K, V]
    def getValue(k: K): V
  }

  // simplistic implementation of a store, by function composition
  case class SimpleStore[I, K, V](i: I, values: K => V) extends Store[I, K, V] {
    override def getInfo = i
    override def putInfo(ni: I) = copy(i = ni)
    override def putValue(k: K, v: V) = SimpleStore(i, key => if (k == key) { v } else values(key))
    override def getValue(k: K) = values(k)
  }

  trait Task[C[_[_]], K, V] {
    def run[F[_]](fetch: K => F[V])(implicit ev: C[F]): F[V]
  }

  trait Tasks[C[_[_]], K, V] {
    def get: K => Option[Task[C, K, V]]
  }

  trait Build[C[_[_]], K, V, I] {
    def build(tasks: Tasks[C, K, V], key: K, store: Store[I, K, V]): Store[I, K, V]
  }

  // a suspending builder
  def busy[K, V, I] = new Build[Applicative, K, V, I]() {
    import MonadInstances._
    import MonadSyntax._

    def build(tasks: Tasks[Applicative, K, V], key: K, store: Store[I, K, V]): Store[I, K, V] = {
      def fetch(k: K): State[Store[I, K, V], V] = tasks.get(k) match {
        case None => State(cs => (cs, store.getValue(k)))
        case Some(task) =>
          for {
            v <- task.run(fetch)
            store <- State.getState[Store[I, K, V]]
            ns <- State.setState(store.putValue(k, v))
          } yield v
      }

      State.execState(fetch(key), store)
    }
  }

  def deps[K, V, I](task: Task[Applicative, K, V]): List[K] = {
    import ApplicativeInstances._
    import MonoidInstances._

    task.run(k => Const[List[K], V](List(k))).m
  }
  
}

object Example {
  import Tasks._

  // you can define you build task for any Applicative
  //
  // def A1[K[_]](implicit ec: Applicative[K]) = new Task[Applicative, K, String, Integer]()(ec) {
  //   def run = key => ec.pure[Integer](0)
  // }

  def workflow = new Tasks[Applicative, String, Integer]() {
    def get = key => key match {
      case "B1" => Some(new Task[Applicative, String, Integer]() {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] = {
          Applicative[F].map2(fetch("A1"), fetch("A2"), _ + _)
        }
      })
      case "B2" => Some(new Task[Applicative, String, Integer]() {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] = {
          Applicative[F].map2(fetch("B1"), Applicative[F].pure(2), _ * _)
        }
      })
      case _ => None
    }
  }
}
object Main {
  def main(args: Array[String]): Unit = {
    import MonadInstances._
    import Tasks._
    import MonadSyntax._
    
    val store: Store[Unit, String, Integer] = SimpleStore((),  (k => if (k == "A1") 10 else 20))
    val result = Tasks.busy.build(Example.workflow, "B2", store)
    println(result.getValue("B1"))

    println(deps(Example.workflow.get("B1").get))
  }
}