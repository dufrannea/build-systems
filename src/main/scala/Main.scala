package main

import cats._
import cats.implicits._
import cats.data.State
import tasks._

object Example {
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
          (fetch("A1"), fetch("A1"), fetch("B1"), Applicative[F].pure(2)).mapN(_ + _ + _ * _)
        }
      })
      case _ => None
    }
  }

  def sprsh2 = new Tasks[Monad, String, Integer]() {
    def get = key => key match {
      case "B1" => Some(new Task[Monad, String, Integer] {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Monad[F]): F[Integer] = {
          fetch("C1").flatMap({
            case 1 => fetch("C2")
            case _ => fetch("A2")
          })
        }
      })
      case "B2" => Some(new Task[Monad, String, Integer] {
        def run[F[_]](fetch: String => F[Integer])(implicit ec: Monad[F]): F[Integer] = {
          fetch("C1").flatMap({
            case 1 => fetch("A1")
            case _ => fetch("B1")
          })
        }
      })
      case _ => None
    }
  }
}

def main(args: Array[String]): Unit = {
  
  import cats.effect._

  val store: Store[Unit, String, Integer] = SimpleStore((),  (k => if (k == "A1") 10 else 20))
  val result = tasks.topological.build(Example.workflow2, "B2", store)
  println(s"B1 ${result.getValue("B1")}")
  println(s"B2 ${result.getValue("B2")}")
  println(deps(Example.workflow2.get("B2").get))
  println(transitiveDeps("B2", Example.workflow2))

  val s = track(Example.sprsh2.get("B1").get, k => IO { println(k); scala.io.StdIn.readInt })

  println("result" + s.unsafeRunSync._1)
}