package duff.build.busy

import scala.language.implicitConversions

import cats.*
import cats.data.State
import cats.implicits.*
import cats.mtl.Stateful
import cats.mtl.implicits.*

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
