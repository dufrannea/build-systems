// Applicative typeclass
trait Applicative[F[_]] {
  def map[A, B](a: F[A], f: A => B): F[B]
  def pure[A](a: A): F[A]
  def ap[A, B](f: F[A => B], a: F[A]): F[B]
  def map2[A, B, C](a: F[A], b: F[B], f: (A, B) => C): F[C] = ap(ap(pure((x: A) => (y: B) => f(x, y)), a), b)
}

object Applicative {
  def apply[F[_] : Applicative] : Applicative[F] = implicitly
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

object State {
  def runState[S, A](s: State[S, A], initialState: S): (S, A) = s.f(initialState)
  def execState[S, A](s: State[S, A], initialState: S): S = s.f(initialState)._1
  def setState[S](s: S): State[S, Unit] = State(cs => (s, ()))
  def getState[S]: State[S, S] = State(cs => (cs, cs))
  def gets[S, A](f: S => A): State[S, A] = State(s => (s, f(s)))
}
// State monad
case class State[S, A](f: S => (S, A))

object StateMonad {
  def stateMonadS[S]: Monad[[A] => State[S, A]] = new Monad[[A] => State[S, A]] {
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
    override def putValue(k: K, v: V) = copy(values = key => if (k == key) { v } else values(k))
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
    // implicit val didier : Applicative[([A] => State[Tasks.Store[I, K, V], A])] = ???
    // implicit val ev :  = ???
    import StateMonad._

    def build(tasks: Tasks[Applicative, K, V], key: K, store: Store[I, K, V]): Store[I, K, V] = {
      def fetch(k: K): State[Store[I, K, V], V] = tasks.get(key) match {
        case None => State(cs => (cs, store.getValue(k)))
        case Some(task) => {
          val ev: Monad[[P] => State[Tasks.Store[I, K, V], P]] = StateMonad.stateMonadS[Tasks.Store[I, K, V]]

          ev.flatMap(
            task.run(fetch)(ev), 
            v => ev.flatMap(
              State.getState, 
                store => 
                  ev.flatMap(State.setState(store.putValue(k, v)), _ => {
                    ev.pure(v)
                  })))
        }
      }

      State.execState(fetch(key), store)
    }
  }
}

object Example {
  import Tasks._

  // you can define you build task for any Applicative
  //
  // def A1[K[_]](implicit ec: Applicative[K]) = new Task[Applicative, K, String, Integer]()(ec) {
  //   def run = key => ec.pure[Integer](0)
  // }

  def workflow= new Tasks[Applicative, String, Integer]() {
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
    import StateMonad._
    import Tasks._
    
    val store: Store[Unit, String, Integer] = SimpleStore((),  (k => if (k == "A1") 10 else 20))
    val result = Tasks.busy.build(Example.workflow, "B2", store)
    println(result.getValue("B1"))
  }
}