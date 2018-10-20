package com.mediarithmics

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.mediarithmics.tag.TLong

import scala.collection.JavaConverters._

trait UserId

class UserService[F[_], EM](implicit DB: DB[F, EM], M : Monad[F]) {

  def createUser(name: String): F[User] =
    DB.transactionally {
      val user = new User(name)
      DB.persist(user).as(user)
    }


  def getUser(userId: TLong[UserId]): F[User] =
    DB.findById[User](userId)

  def addUserToGroup(userId: TLong[UserId], groupId: TLong[GroupId]): F[Unit] =
    DB.transactionally {
      for {
        user <- getUser(userId)
        group <- DB.findById[Group](groupId)
        _ = user.addGroup(group)
        _ <- DB.persist(user)

      } yield ()
    }

  def getUserGroups(userId: TLong[UserId]): F[List[Group]] =
    DB.transactionally {
      getUser(userId).map(_.getGroups.asScala.toList)
    }
}
