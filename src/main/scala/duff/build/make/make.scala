package duff.build.make 

import scala.language.implicitConversions
import cats.*
import cats.data.*
import cats.implicits.*
import cats.mtl.{*, given}
import duff.build.core.{Rebuilder, Task}

type Time = Integer
type MakeInfo[K] = (Time, Map[K, Time])

def modTimeRebuilder[K, V]: Rebuilder[Applicative, MakeInfo[K], K, V] =
  (k: K, v: V, t: Task[Applicative, K, V]) =>
    new Task[[m[_]] =>> Stateful[m, MakeInfo[K]], K, V] {

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
