package com.mediarithmics

import cats.data.Kleisli
import cats.effect.{ExitCase, Sync}
import cats.syntax.functor._
import javax.persistence.{EntityTransaction, EntityManager => JEM}

import scala.reflect.ClassTag

object DB2 {


  trait EntityManager[F[_]] {

    def findById[A: ClassTag](id: Long): F[A]

    def persist(a: Any): F[Unit]

    def merge[A](a: A): F[A]

    def remove[A <: EntityWithId : ClassTag](e: A): F[Unit]

  }

  object EntityManager {
    def instance[F[_]](em: JEM)
                      (implicit S: Sync[F]): EntityManager[F] = new EntityManager[F] {

      override def findById[A: ClassTag](id: Long): F[A] = S.suspend {
        val clazz = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
        Option(em.find(clazz, id)).map(S.pure[A])
          .getOrElse(S.raiseError(MicsError(s"unknown ${clazz.getSimpleName} with id $id")))
      }

      override def persist(a: Any): F[Unit] =
        S.delay {
          em.persist(a)
          em.flush()
        }

      override def merge[A](a: A): F[A] =
        S.delay {
          em.merge(a)
          em.flush()
          a
        }

      override def remove[A <: EntityWithId : ClassTag](e: A): F[Unit] =
        S.suspend {
          if (em contains e)
            S.delay(em remove e)
          else for {
            _ <- findById(e.getId())
            _ = em remove e
          } yield ()
        }
    }
  }


  trait DB[F[_]] {

    def transactionally[A](fa: EntityManager[F] => F[A]): F[A]

    def transactionally[A](fa: => F[A]): F[A] = transactionally(_ => fa)

    def findById[A: ClassTag](id: Long): F[A] = transactionally(_.findById(id))

    def persist(a: Any): F[Unit] = transactionally(_.persist(a))

    def merge[A](a: A): F[A] = transactionally(_.merge(a))

    def remove[A <: EntityWithId : ClassTag](e: A): F[Unit] = transactionally(_.remove(e))

  }

  object DB {
    //type TransactionState[M[_]] = Option[(EntityManager[Transactionable[M, ?]], EntityTransaction)] //illegal cyclic reference, Fix ?
    type TransactionState = Option[(JEM, EntityTransaction)]
    type Transactionable[M[_], A] = Kleisli[M, TransactionState, A]


    private def transactionBracket[F[_], A](em: JEM,
                                            fa: EntityManager[Transactionable[F, ?]] => Transactionable[F, A])
                                           (implicit S: Sync[Transactionable[F, ?]]): Transactionable[F, A] = {

      val acquire: Transactionable[F, EntityTransaction] =
        S.delay {
          println("acquiring tx")
          val ts = em.getTransaction
          ts.begin()
          ts
        }

      val use: EntityTransaction => Transactionable[F, A] =
        t =>
          Kleisli.local[F, A, TransactionState] {
            _ =>
              println("using it")
              Some((em, t))
          }(fa(EntityManager.instance(em)))
      //_ => fa(em)

      val release: (EntityTransaction, ExitCase[Throwable]) => Transactionable[F, Unit] = {
        case (t, ExitCase.Completed) =>
          S.delay {
            println("release tx - success")
            t.commit()
            em.close()
          }
        case (_, ExitCase.Canceled) =>
          S.delay {
            //            Thread.currentThread().getStackTrace.map(println)
            println("release - Cancel")
            //            t.rollback()
            //            em.close()
          }
        case (t, ExitCase.Error(err)) =>
          S.delay {
            println(err.getMessage)
            println("release tx - failure")
            t.rollback()
            em.close()
          }
      }

      S.bracketCase[EntityTransaction, A](acquire)(use)(release)

    }

    def instance[F[_]](getEntityManager: () => JEM)
                      (implicit S: Sync[F]): DB[Transactionable[F, ?]] =
      new DB[Transactionable[F, ?]] {
        override def transactionally[A](fa: EntityManager[Transactionable[F, ?]] => Transactionable[F, A]): Transactionable[F, A] =
          for {
            // remember kleisli aka ReaderT abstract a funtion of type A => F[B]
            // in our case, A = TransactionState
            // ask let us retrieve the fed parameter
            state <- Kleisli.ask[F, TransactionState]
            result <-
              state match {
                case Some((em, _)) =>
                  fa(EntityManager.instance(em))
                case None =>
                  println("need transaction !")
                  transactionBracket(getEntityManager(), fa)(Sync.catsKleisliSync[F, TransactionState])
              }
          } yield result
      }

  }

}
