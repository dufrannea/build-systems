package tasks

import cats._
import cats.data.State
import cats.implicits._
import cats.mtl.Stateful
import cats.mtl.implicits._

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

  override def putValue(k: K, v: V) = SimpleStore(
    i,
    key =>
      if (k == key) { v }
      else values(key)
  )

  override def getValue(k: K) = values(k)
}

/** Given type constraint C, and functor F that follows this type constraint, provides the ability to retrieve a key
  * value in the context of F.
  */
trait Task[C[_[_]], K, V] {
  def run[F[_]: C](fetch: K => F[V]): F[V]
}

/** Structure representing all the tasks considered. Contract is simple: for a given key, return the corresponding build
  * task if any.
  */
trait Tasks[C[_[_]], K, V] {
  def get: K => Option[Task[C, K, V]]
}

/** A build respecting the constraint C.
  *
  * Takes a definition of all tasks, an initial store, and returns a new value of the store updated by building the key
  * K.
  */
trait Build[C[_[_]], K, V, I] {
  def build(tasks: Tasks[C, K, V], key: K, store: Store[I, K, V]): Store[I, K, V]
}

def getState[V]: State[V, V] = State(s => (s, s))
def putState[A](a: A): State[A, Unit] = State(_ => (a, ()))

// a suspending builder
def busy[K, V, I] = new Build[Applicative, K, V, I]() {

  def build(tasks: Tasks[Applicative, K, V], key: K, store: Store[I, K, V]): Store[I, K, V] = {
    def fetch(k: K): State[Store[I, K, V], V] = tasks.get(k) match {
      case None       => State(cs => (cs, store.getValue(k)))
      case Some(task) =>
        for {
          v     <- task.run(fetch)
          store <- getState
          _     <- putState(store.putValue(k, v))
        } yield v
    }

    fetch(key).run(store).value._1
  }

}

def dependencies[K, V](task: Task[Applicative, K, V]): List[K] = {
  import cats.data.Const

  task.run(k => Const[List[K], V](List(k))).getConst
}

def track[M[_]: Monad, K, V](t: Task[Monad, K, V], fetch: K => M[V]): M[(List[(K, V)], V)] = {
  import cats.data.WriterT

  def writerFetch(k: K): WriterT[M, List[(K, V)], V] = WriterT
    .liftF[M, List[(K, V)], V](fetch(k))
    .flatMap { v =>
      WriterT
        .tell[M, List[(K, V)]](List(k -> v))
        .map(_ => v)
    }

  t.run(writerFetch).run
}

/*
 * Get all transitive *task* dependencies in topological order
 */
def transitiveDeps[Ka, Va](key: Ka, tasks: Tasks[Applicative, Ka, Va]): List[Ka] =
  tasks.get(key) match {
    case None       => List()
    case Some(task) => (dependencies[Ka, Va](task).flatMap(transitiveDeps(_, tasks)) ++ List(key)).distinct
  }

trait Rebuilder[C[_[_]], IR, K, V] {
  def wrap(k: K, v: V, t: Task[C, K, V]): Task[[M[_]] =>> Stateful[M, IR], K, V]
}

trait Scheduler[C[_[_]], I, IR, K, V] {
  def toBuild(r: Rebuilder[C, IR, K, V]): Build[C, K, V, I]
}

// partially applied monadstate to avoid
// writing type lambdas
type MonadStateK[K] = [M[_]] =>> Stateful[M, K]

def dummyRebuilder[K, V] = new Rebuilder[Applicative, Unit, K, V] {

  def wrap(k: K, v: V, t: Task[Applicative, K, V]): Task[MonadStateK[Unit], K, V] =
    new Task[MonadStateK[Unit], K, V] {

      def run[F[_]: MonadStateK[Unit]](fetch: (K => F[V])): F[V] = {
        val tartine: Stateful[F, Unit] = implicitly[Stateful[F, Unit]]
        t.run[F](fetch)(tartine.monad)
      }

    }

}

def topological[K, V, I]: Scheduler[Applicative, I, I, K, V] = new Scheduler {

  def liftStore[A](x: State[I, A]): State[Store[I, K, V], A] =
    State[Store[I, K, V], A] { store =>
      val (i, a) = x.run(store.getInfo).value
      store.putInfo(i) -> a
    }

  def toBuild(r: Rebuilder[Applicative, I, K, V]) =
    new Build[Applicative, K, V, I] {
      import scala.language.implicitConversions

      def build(
        tasks: Tasks[Applicative, K, V],
        key: K,
        store: Store[I, K, V]
      ): Store[I, K, V] = {

        val order = transitiveDeps[K, V](key, tasks)

        def build0(k: K): State[Store[I, K, V], Unit] = tasks.get(k) match {
          case None    => State(s => (s, ()))
          case Some(t) =>
            for {
              store    <- getState[Store[I, K, V]]
              value = store.getValue(k)
              newTask = r.wrap(k, value, t)
              fetch: (K => State[I, V]) = k => State(s => (s, store.getValue(k)))
              newValue <- liftStore(newTask.run(fetch))
              _        <- cats.data.State.modify[Store[I, K, V]](_.putValue(k, newValue))
            } yield ()
        }

        order.traverse_(build0).runS(store).value
      }

    }

}
