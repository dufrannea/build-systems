package make

import scala.language.implicitConversions

import cats._
import cats.data._
import cats.implicits._
import cats.mtl.{given, *}
import cats.mtl.Stateful

import tasks._

type Time = Integer
type MakeInfo[K] = (Time, Map[K, Time])

type MonadStateK[U] = [M[_]] =>> Stateful[M, U]

def modTimeRebuilder[K, V]: Rebuilder[Applicative, MakeInfo[K], K, V] =
  (k: K, v: V, t: Task[Applicative, K, V]) =>
    new Task[MonadStateK[MakeInfo[K]], K, V] {

      def run[F[_]](fetch: K => F[V])(using F: Stateful[F, MakeInfo[K]]): F[V] = {
        implicit val lol = F.monad

        for {
          get <- F.get
          (now, map) = get
          dirty = map.get(k) match {
                    case None       => true
                    case Some(time) =>
                      dependencies(t)
                        .exists(depKey => map.get(depKey).exists(t => t > time))
                  }
          res <- if (dirty) {
                   for {
                     _ <- F.set((now + 1, map + (k -> now)))
                     r <- t.run(fetch)
                   } yield r
                 } else {
                   F.monad.pure(v)
                 }
        } yield res
      }

    }
