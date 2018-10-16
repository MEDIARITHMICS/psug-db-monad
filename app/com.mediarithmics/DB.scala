package com.mediarithmics

import cats.data.{Kleisli, ReaderT}
import cats.effect._
import cats.{MonadError, Monoid}
import com.mediarithmics.DB.TransactionableIO.TransactionableIO
import javax.persistence._

import scala.reflect.ClassTag
import scala.util.control.NonFatal

/*
 * The DB type class interface
 * - exposes the DB/Hibernate specific operations
 * - extends the Effect type class interface to request
 *    the handling of synchronous/asynchronous control flow
 *    and of exceptions
 */

case class MicsError(msg: String) extends Error(msg)

trait DB[F[_]] extends Effect[F] {

  def transactionally[A](fa: => F[A]): F[A]

  def findById[A: ClassTag](id: Long): F[A]

  def persist(a: Any): F[Unit]

  def merge[T](a: T): F[T]

  def remove[A <: EntityWithId : ClassTag](e: A): F[Unit]

  //  def withEntityManager[A](fa: EntityManager => A): F[A]
  //
  //  def withEntityManagerF[A](fa: EntityManager => F[A]): F[A]
  //
  //  def withEntityManagerIO[A](fa: EntityManager => IO[A]): F[A] = withEntityManagerF(fa andThen liftIO)

}

object DB {

  type TransactionState = Option[(EntityManager, EntityTransaction)]
  type TransactionM[M[_], A] = Kleisli[M, TransactionState, A]

  // the Transaction state monoid is required only to provide the initial empty state of a transaction
  val TransactionStateMonoid: Monoid[TransactionState] = new Monoid[TransactionState] {
    override def empty: TransactionState = None

    override def combine(x: TransactionState, y: TransactionState): TransactionState =
      throw MicsError("TransactionStateMonoid should be used only for its empty value")
  }

  def apply[F[_] : DB]: DB[F] = implicitly

  type ThrowMonad[F[_]] = MonadError[F, Throwable]


  def transactionableInstance[M[_]](getEntityManager: () => EntityManager,
                                    delegate: Effect[TransactionM[M, ?]])
                                   (implicit te: Effect[M]): DB[TransactionM[M, ?]] = new DB[TransactionM[M, ?]] {

    //implicit val tm: ThrowMonad[TransactionM[M, ?]] = this

    private def transactionBracket[A](em: EntityManager)
                                     (fa: EntityManager => TransactionM[M, A]): TransactionM[M, A] = {
      val acquire: TransactionM[M, EntityTransaction] =
        for {
          acquired <- suspend {
            println("acquiring tx")
            try {
              val ts = em.getTransaction
              ts.begin()
              pure(ts)

            }catch {
              case NonFatal(err) =>
                println(s"dafuk ?? ${err.getMessage}")
                throw err
            }
          }
        } yield acquired

      val use: EntityTransaction => TransactionM[M, A] =
        t =>
          Kleisli.local[M, A, TransactionState] {
            case Some(_) =>
              println("guess what ?")
              throw MicsError("a transaction is already present")
            case _ => println("using it")
              Some((em, t))
          }(fa(em))
      //_ => fa(em)

      val release: (EntityTransaction, ExitCase[Throwable]) => TransactionM[M, Unit] = {
        case (t, ExitCase.Completed) =>
          delay {
            println("release tx - success")
            t.commit()
            em.close()
          }
        case (_,  ExitCase.Canceled) =>
          delay {
//            Thread.currentThread().getStackTrace.map(println)
            println("release - Cancel")
//            t.rollback()
//            em.close()
          }
        case (t, ExitCase.Error(err)) =>
          delay {
            println(err.getMessage)
            println("release tx - failure")
            t.rollback()
            em.close()
          }
      }

      bracketCase[EntityTransaction, A](acquire)(use)(release)

    }


    override def transactionally[A](fa: => TransactionM[M, A]): TransactionM[M, A] =
      withEntityManagerF(_ => fa)

    def withEntityManager[A](fa: EntityManager => A): TransactionM[M, A] =
      withEntityManagerF(em => delegate.lift(fa).apply(delegate.pure(em)))

    def withEntityManagerF[A](fa: EntityManager => TransactionM[M, A]): TransactionM[M, A] =
      for {
        // remember kleisli aka ReaderT abstract a funtion of type A => F[B]
        // in our case, A = TransactionState
        // ask let us retrieve the fed parameter
        st <- Kleisli.ask[M, TransactionState]
        result <-
          st match {
            case Some((em, _)) =>
              println("already in a transaction !")
              fa(em)
            case None =>
              println("need transaction !")

              transactionBracket(getEntityManager())(fa)
          }
      } yield result


    override def findById[A: ClassTag](id: Long): TransactionM[M, A] =
      withEntityManagerF(entityManager =>
        Option(entityManager.find(implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]], id)).map(delegate.pure[A])
          .getOrElse(raiseError(MicsError(s"unknown ${implicitly[ClassTag[A]].runtimeClass} with id $id"))))


    def persist(a: Any): TransactionM[M, Unit] =
      withEntityManager {
        em =>
          println("persisting")
          em.persist(a)
          println("flushing")
          em.flush()
      }

    def merge[T](a: T): TransactionM[M, T] =
      withEntityManager {
        em =>
          em.merge(a)
          em.flush()
          a
      }

    def remove[A <: EntityWithId : ClassTag](e: A): TransactionM[M, Unit] = withEntityManagerF {
      em =>
        if (em contains e)
          delay(em remove e)
        else for {
          _ <- findById(e.getId())
          _ = em remove e
        } yield ()
    }

    override def runAsync[A](fa: TransactionM[M, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
      delegate.runAsync(fa)(cb)

    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): TransactionM[M, A] =
      delegate.async(k)

    override def suspend[A](thunk: => TransactionM[M, A]): TransactionM[M, A] = thunk

    override def raiseError[A](e: Throwable): TransactionM[M, A] =
      delegate.raiseError[A](e)

    override def handleErrorWith[A](fa: TransactionM[M, A])(f: Throwable => TransactionM[M, A]): TransactionM[M, A] =
      delegate.handleErrorWith(fa)(f)

    override def pure[A](x: A): TransactionM[M, A] = delegate.pure(x)

    override def flatMap[A, B](fa: TransactionM[M, A])(f: A => TransactionM[M, B]): TransactionM[M, B] =
      delegate.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => TransactionM[M, Either[A, B]]): TransactionM[M, B] =
      delegate.tailRecM(a)(f)

    override def asyncF[A](k: (Either[Throwable, A] => Unit) => TransactionM[M, Unit]): TransactionM[M, A] =
      delegate.asyncF(k)

    override def bracketCase[A, B](acquire: TransactionM[M, A])
                                  (use: A => TransactionM[M, B])
                                  (release: (A, ExitCase[Throwable]) => TransactionM[M, Unit]): TransactionM[M, B] =
      delegate.bracketCase(acquire)(use)(release)

  }


  object TransactionableIO {

    type TransactionableIO[A] = TransactionM[IO, A]

    def apply[A](a: => A): TransactionableIO[A] = Kleisli.liftF[IO, TransactionState, A](IO(a))

    def apply[A](ioa: IO[A]): TransactionableIO[A] = Kleisli.liftF[IO, TransactionState, A](ioa)

    def transact[A](db: TransactionableIO[A]): IO[A] = db(None)

    def unsafeRunSyncTransacted[A](db: TransactionableIO[A]): A =
      transact(db).unsafeRunSync()

    implicit class TransactionableIOSyntax[A](val transactedIO: TransactionableIO[A]) extends AnyVal {
      def unsafeRunSyncTransacted(): A = TransactionableIO.unsafeRunSyncTransacted(transactedIO)

      def transact: IO[A] = TransactionableIO.transact(transactedIO)
    }

  }

  // missing piece, should be found in cats-effect. We could do a PR
  def readerEffect[F[_], R](implicit e: Effect[F],
                            mr: Monoid[R]): Effect[Kleisli[F, R, ?]] = {
    type K[A] = Kleisli[F, R, A]

    new Effect[K] {

      val me: MonadError[K, Throwable] = Kleisli.catsDataMonadErrorForKleisli(e)

      override def runAsync[A](fa: ReaderT[F, R, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
        e.runAsync(fa.run(mr.empty))(cb)

      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ReaderT[F, R, A] =
        Kleisli.liftF(e.async(k))

      override def suspend[A](thunk: => ReaderT[F, R, A]): ReaderT[F, R, A] = thunk

      override def flatMap[A, B](fa: ReaderT[F, R, A])(f: A => ReaderT[F, R, B]): ReaderT[F, R, B] =
        me.flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => ReaderT[F, R, Either[A, B]]): ReaderT[F, R, B] =
        me.tailRecM(a)(f)

      override def raiseError[A](e: Throwable): ReaderT[F, R, A] = me.raiseError(e)

      override def handleErrorWith[A](fa: ReaderT[F, R, A])(f: Throwable => ReaderT[F, R, A]): ReaderT[F, R, A] =
        me.handleErrorWith(fa)(f)

      override def pure[A](x: A): ReaderT[F, R, A] =
        Kleisli.pure[F, R, A](x)

      override def asyncF[A](k: (Either[Throwable, A] => Unit) => K[Unit]): K[A] =
        Kleisli.liftF[F, R, A](e.asyncF(k.andThen(ku => ku.run(mr.empty))))

      override def bracketCase[A, B](acquire: K[A])(use: A => K[B])(release: (A, ExitCase[Throwable]) => K[Unit]): K[B] =
        Kleisli.liftF(e.bracketCase(acquire.run(mr.empty))(a => use(a).run(mr.empty))((a, ec) => release(a, ec).run(mr.empty)))

    }
  }

  def ioInstance(entityManager: () => EntityManager)
                (implicit contextShift: ContextShift[IO]): DB[TransactionableIO] =
    transactionableInstance[IO](
      entityManager,
      readerEffect[IO, TransactionState](IO.ioConcurrentEffect, TransactionStateMonoid))(IO.ioConcurrentEffect)

}
