import cats._
import cats.implicits._
import cats.data.State

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

  def topological[K, V, I] = new Build[Applicative, K, V, I] {
    import scala.language.implicitConversions

    /*
     * Get all transitive deps in topological order
     */
    def transitiveDeps[Ka, Va](key: Ka, tasks: Tasks[Applicative, Ka, Va]): List[Ka] =
      for {
        task   <- tasks.get(key).toList
        dep    <- deps[Ka, Va](task)
        trDep  <- (transitiveDeps(dep, tasks) ++ List(dep)).toSet.toList // TODO: optimize with visited
      } yield trDep

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
  
}

object Example {
  import Tasks._
  import scala.language.implicitConversions

  // you can define you build task for any Applicative
  //
  // def A1[K[_]](implicit ec: Applicative[K]) = new Task[Applicative, K, String, Integer]()(ec) {
  //   def run = key => ec.pure[Integer](0)
  // }

  def workflow = new Tasks[Applicative, String, Integer]() {
    def get = key => key match {
      case "B1" => Some(new Task[Applicative, String, Integer]() {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] = {
          (fetch("A1"), fetch("A2")).mapN(_ + _)
        }
      })
      case "B2" => Some(new Task[Applicative, String, Integer]() {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] = {
          (fetch("B1"), Applicative[F].pure(2)).mapN(_ * _)
        }
      })
      case _ => None
    }
  }

  def workflow2 = new Tasks[Applicative, String, Integer]() {
    def get = key => key match {
      case "B1" => Some(new Task[Applicative, String, Integer]() {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] = {
          (fetch("A1"), fetch("A2")).mapN(_ + _)
        }
      })
      case "B2" => Some(new Task[Applicative, String, Integer]() {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] = {
          (fetch("A1"), fetch("B1"), Applicative[F].pure(2)).mapN(_ + _ * _)
        }
      })
      case _ => None
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import Tasks._
    
    val store: Store[Unit, String, Integer] = SimpleStore((),  (k => if (k == "A1") 10 else 20))
    val result = Tasks.topological.build(Example.workflow2, "B2", store)
    println(s"B1 ${result.getValue("B1")}")
    println(s"B2 ${result.getValue("B2")}")

    println(deps(Example.workflow2.get("B2").get))
  }
}