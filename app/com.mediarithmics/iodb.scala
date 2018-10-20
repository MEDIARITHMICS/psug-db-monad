package com.mediarithmics

import cats.MonadError
import cats.data.Kleisli
import cats.effect._
import javax.persistence.EntityManager


case class MicsError(msg: String) extends Error(msg)

object iodb {


  type ThrowMonad[F[_]] = MonadError[F, Throwable]


  import DB.Transactioner._

  type TransactionableIO[A] = Transactionable[IO, A]

  def apply[A](a: => A): TransactionableIO[A] = Kleisli.liftF[IO, TransactionState, A](IO(a))

  def apply[A](ioa: IO[A]): TransactionableIO[A] = Kleisli.liftF[IO, TransactionState, A](ioa)

  def transact[A](db: TransactionableIO[A]): IO[A] = db(None)

  def unsafeRunSyncTransacted[A](db: TransactionableIO[A]): A =
    transact(db).unsafeRunSync()

  implicit class TransactionableIOSyntax[A](val transactedIO: TransactionableIO[A]) extends AnyVal {
    def unsafeRunSyncTransacted(): A = iodb.unsafeRunSyncTransacted(transactedIO)

    def transact: IO[A] = iodb.transact(transactedIO)
  }


  def ioInstance(entityManager: () => EntityManager)
                (implicit contextShift: ContextShift[IO]): DB[TransactionableIO, EntityManager] = {
    implicit val tx: DB.Transactioner[Transactionable[IO, ?], EntityManager] =
      DB.Transactioner.instance[IO](entityManager)
    implicit val em: DB.EntityManager[Transactionable[IO, ?], EntityManager] =
      DB.EntityManager.instance(Sync.catsKleisliSync[IO, TransactionState])

    new DB[TransactionableIO, EntityManager]

  }


}
