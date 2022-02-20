package duff.build 

import scala.language.implicitConversions
import cats.*
import cats.data.State
import cats.effect.*
import cats.implicits.*
import duff.build.core.{SimpleStore, Store, Task, Tasks}

object Example {

  // you can define you build task for any Applicative
  //
  // def A1[K[_]](implicit ec: Applicative[K]) = new Task[Applicative, K, String, Integer]()(ec) {
  //   def run = key => ec.pure[Integer](0)
  // }

  def workflow = new Tasks[Applicative, String, Integer]() {

    def get = key =>
      key match {
        case "B1" =>
          Some(new Task[Applicative, String, Integer]() {
            def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] =
              (fetch("A1"), fetch("A2")).mapN(_ + _)
          })
        case "B2" =>
          Some(new Task[Applicative, String, Integer]() {
            def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] =
              (fetch("B1"), Applicative[F].pure(2)).mapN(_ * _)
          })
        case _    => None
      }

  }

  def workflow2 = new Tasks[Applicative, String, Integer]() {

    def get = key =>
      key match {
        case "B1" =>
          Some(new Task[Applicative, String, Integer]() {
            def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] =
              (fetch("A1"), fetch("A2")).mapN(_ + _)
          })
        case "B2" =>
          Some(new Task[Applicative, String, Integer]() {
            def run[F[_]](fetch: String => F[Integer])(implicit ec: Applicative[F]): F[Integer] =
              (fetch("A1"), fetch("A1"), fetch("B1"), Applicative[F].pure(2)).mapN(_ + _ + _ * _)
          })
        case _    => None
      }

  }

  def sprsh2 = new Tasks[Monad, String, Integer]() {

    def get = key =>
      key match {
        case "B1" =>
          Some(new Task[Monad, String, Integer] {

            def run[F[_]](fetch: String => F[Integer])(implicit ec: Monad[F]): F[Integer] =
              fetch("C1").flatMap {
                case 1 => fetch("C2")
                case _ => fetch("A2")
              }

          })
        case "B2" =>
          Some(new Task[Monad, String, Integer] {

            def run[F[_]](fetch: String => F[Integer])(implicit ec: Monad[F]): F[Integer] =
              fetch("C1").flatMap {
                case 1 => fetch("A1")
                case _ => fetch("B1")
              }

          })
        case _    => None
      }

  }

}

object Main extends IOApp.Simple {

  import cats.effect.*

  val run = {
    val store: Store[Unit, String, Integer] = SimpleStore(
      (),
      {
        case "A1" => 10
        case "A2" => 20
        case r    => 42
      }
    )

    println("**** BUSY *****")
    val resultBusy = busy.build(Example.workflow2, "B2", store)
    println(s"B1 ${resultBusy.getValue("B1")}")
    println(s"B2 ${resultBusy.getValue("B2")}")

    println("**** TOPO / dummyRebuilder *****")
    val result = tasks.topological.toBuild(tasks.dummyRebuilder).build(Example.workflow2, "B2", store)
    println(s"B1 ${result.getValue("B1")}")
    println(s"B2 ${result.getValue("B2")}")
    println(dependencies(Example.workflow2.get("B2").get))
    println(transitiveDeps("B2", Example.workflow2))

    // make: topological scheduler + modTimeRebuilder
    println("**** Make : TOPO / modTimeRebuilder *****")
    import make._
    val makeBuild = topological[String, Integer, (Time, Map[String, Time])].toBuild(modTimeRebuilder)

    val makeStore: Store[(Time, Map[String, Time]), String, Integer] =
      SimpleStore(
        (1, Map.empty),
        {
          case "A1" => 10
          case "A2" => 20
          case r    => 42
        }
      )

    val makeResult = makeBuild.build(Example.workflow2, "B2", makeStore)

    println(makeResult.getInfo._2)
    println(s"B1 ${makeResult.getValue("B1")}")
    println(s"B2 ${makeResult.getValue("B2")}")

    // tracking dynamic dependencies
    track(
      Example.sprsh2.get("B1").get,
      k =>
        IO {
          // could be a read from command line
          1
        }
    ).map(r => println(s"result is ${r._1}")).as(ExitCode.Success)

  }

}
