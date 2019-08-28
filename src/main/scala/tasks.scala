package tasks

import cats._
import cats.implicits._
import cats.data.State

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

def getState[V]: State[V,V] = State(s => (s,s))
def putState[A](a:A): State[A, Unit] = State(_ => (a, ()))

// a suspending builder
def busy[K, V, I] = new Build[Applicative, K, V, I]() {
  def build(tasks: Tasks[Applicative, K, V], key: K, store: Store[I, K, V]): Store[I, K, V] = {
    def fetch(k: K): State[Store[I, K, V], V] = tasks.get(k) match {
      case None => State(cs => (cs, store.getValue(k)))
      case Some(task) =>
        for {
          v <- task.run(fetch)
          store <- getState
          _ <- putState(store.putValue(k, v))
        } yield v
    }

    fetch(key).run(store).value._1
  }
}

def deps[K, V](task: Task[Applicative, K, V]): List[K] = {
  import cats.data.Const

  task.run(k => Const[List[K], V](List(k))).getConst
}

def track[M[_] : Monad, K, V](t: Task[Monad, K, V], fetch: K => M[V]): M[(List[(K, V)], V)] = {
  import cats.data.WriterT

  def writerFetch(k: K): WriterT[M, List[(K,V)], V] = WriterT.liftF[M, List[(K,V)], V](fetch(k))
    .flatMap { v => WriterT.tell[M, List[(K,V)]](List(k -> v))
    .map(_ => v) }

  t.run(writerFetch).run
}

/*
  * Get all transitive deps in topological order
  */
def transitiveDeps[Ka, Va](key: Ka, tasks: Tasks[Applicative, Ka, Va]): List[Ka] =
    tasks.get(key) match {
      case None => List(key)
      case Some(task) => (deps[Ka, Va](task).flatMap(transitiveDeps(_, tasks)) ++ List(key)).distinct
    }

def topological[K, V, I] = new Build[Applicative, K, V, I] {
  import scala.language.implicitConversions

  def build(tasks: Tasks[Applicative, K, V], key: K, store: Store[I, K, V]): Store[I, K, V] = {
    tasks.get(key) match {
      case None => store
      case Some(t) =>
        val depsStore = transitiveDeps[K, V](key, tasks).flatMap({ a =>
          tasks.get(a).toList.map(_ -> a)
        }).foldLeft(store)({ case (cs, (currentTask, currentKey)) =>
          val v = currentTask.run[Id](k => cs.getValue(k))
          println(s"ran ${currentKey} got ${v}")
          cs.putValue(currentKey, v)
        })

        store.putValue(key, t.run[Id](k => depsStore.getValue(k)))
    }
  }
}