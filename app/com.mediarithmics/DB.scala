package com.mediarithmics

import cats.data.Kleisli
import cats.effect.{ExitCase, Sync}
import cats.syntax.functor._
import com.mediarithmics.DB.{EntityManager, Transactioner}
import javax.persistence.{EntityTransaction, EntityManager => JEM}

import scala.reflect.ClassTag

class DB[F[_], EM: EntityManager[F, ?]: Transactioner[F, ?]] {

  def transactionally[A](fa: => F[A]): F[A] =
    Transactioner[F, EM].transactionally(_ => fa)

  def findById[A: ClassTag](id: Long): F[A] =
    Transactioner[F, EM].transactionally(EntityManager[F, EM].findById(_, id))

  def persist(a: Any): F[Unit] =
    Transactioner[F, EM].transactionally(EntityManager[F, EM].persist(_, a))

  def merge[A](a: A): F[A] =
    Transactioner[F, EM].transactionally(EntityManager[F, EM].merge(_, a))

  def remove[A <: EntityWithId : ClassTag](e: A): F[Unit] =
    Transactioner[F, EM].transactionally(EntityManager[F, EM].remove(_, e))

}

object DB {


  trait EntityManager[F[_], EM] {

    def findById[A: ClassTag](em: EM, id: Long): F[A]

    def persist(em: EM, a: Any): F[Unit]

    def merge[A](em: EM, a: A): F[A]

    def remove[A <: EntityWithId : ClassTag](em: EM, e: A): F[Unit]

  }

  object EntityManager {
    def instance[F[_]](implicit S: Sync[F]): EntityManager[F, JEM] = new EntityManager[F, JEM] {

      override def findById[A: ClassTag](em: JEM, id: Long): F[A] = S.suspend {
        val clazz = implicitly[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]]
        Option(em.find(clazz, id)).map(S.pure[A])
          .getOrElse(S.raiseError(MicsError(s"unknown ${clazz.getSimpleName} with id $id")))
      }

      override def persist(em: JEM, a: Any): F[Unit] =
        S.delay {
          em.persist(a)
          em.flush()
        }

      override def merge[A](em: JEM, a: A): F[A] =
        S.delay {
          em.merge(a)
          em.flush()
          a
        }

      override def remove[A <: EntityWithId : ClassTag](em: JEM, e: A): F[Unit] =
        S.suspend {
          if (em contains e)
            S.delay(em remove e)
          else for {
            _ <- findById(em, e.getId())
            _ = em remove e
          } yield ()
        }
    }

    def apply[F[_], EM: EntityManager[F, ?]]: EntityManager[F, EM] = implicitly
  }


  trait Transactioner[F[_], EM] {
    def transactionally[A](fa: EM => F[A]): F[A]
  }

  object Transactioner {
    //type TransactionState[M[_]] = Option[(EntityManager[Transactionable[M, ?]], EntityTransaction)] //illegal cyclic reference, Fix ?
    type TransactionState = Option[(JEM, EntityTransaction)]
    type Transactionable[M[_], A] = Kleisli[M, TransactionState, A]


    private def transactionBracket[F[_], A](em: JEM,
                                            fa: JEM => Transactionable[F, A])
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
            _ => //inject state
              println("using it")
              Some((em, t))
          }(fa(em))

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
                      (implicit S: Sync[F]): Transactioner[Transactionable[F, ?], JEM] =
      new Transactioner[Transactionable[F, ?], JEM] {
        val KS = Sync.catsKleisliSync[F, TransactionState]
        override def transactionally[A](fa: JEM => Transactionable[F, A]): Transactionable[F, A] =
          for {
            // remember kleisli aka ReaderT abstract a funtion of type A => F[B]
            // in our case, A = TransactionState
            // ask let us retrieve the fed parameter
            state <- Kleisli.ask[F, TransactionState]
            result <-
              state match {
                case Some((em, _)) =>
                  fa(em)
                case None =>
                  println("need transaction !")
                  transactionBracket(getEntityManager(), fa)(KS)
              }
          } yield result
      }

    def apply[F[_], EM: Transactioner[F, ?]]: Transactioner[F, EM] = implicitly
  }


}
