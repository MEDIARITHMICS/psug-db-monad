package com.mediarithmics

import cats.syntax.flatMap._
import cats.syntax.functor._
import com.mediarithmics.tag.TLong

import scala.collection.JavaConverters._

trait UserId

class UserService[F[_]](implicit DB: DB[F]) {

  def createUser(name: String): F[User] =
    DB.transactionally {
      for {
        user <- DB.delay(new User(name))
        _ <- DB.persist(user)
      } yield user
    }


  def getUser(userId: TLong[UserId]): F[User] =
    DB.findById[User](userId)

  def addUserToGroup(userId: TLong[UserId], groupId: TLong[GroupId]): F[Unit] =
    DB.transactionally {
      for {
        user <- getUser(userId)
        group <- DB.findById[Group](groupId)
        _ <- DB.delay(user.addGroup(group))
        _ <- DB.persist(user)

      } yield ()
    }

  def getUserGroups(userId: TLong[UserId]): F[List[Group]] =
    DB.transactionally {
      getUser(userId).map(_.getGroups.asScala.toList)
    }
}
