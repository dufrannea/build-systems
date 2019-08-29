package make

import cats._
import cats.data._
import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.MonadState
import tasks._

type Time = Integer
type MakeInfo[K] = (Time, Map[K, Time])

type MonadStateK[U] = [M[_]] =>> MonadState[M, U]

import scala.language.implicitConversions

def modTimeRebuilder[K, V]: Rebuilder[Applicative, MakeInfo[K], K, V] = 
  (k: K, v: V, t: Task[Applicative, K, V]) => new Task[MonadStateK[MakeInfo[K]], K, V] {
    def run[F[_]](fetch: K => F[V])(implicit F: MonadState[F, MakeInfo[K]]): F[V] = {
      implicit val lol = F.monad

      for {
        get <- F.get
        (now, map) = get
        dirty = map.get(k) match {
          case None => true
          case Some(time) => deps(t).exists(depKey => map.get(depKey).exists(t => t > time))
        }
        res <- if (dirty) {
          F.set((now + 1, map + (k -> now))).flatMap { case _ =>
            t.run(fetch)
          }
        } else {
          F.monad.pure(v)
        }
      } yield res
    }
  }