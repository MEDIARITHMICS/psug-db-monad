package com.mediarithmics

import cats.syntax.flatMap._
import cats.syntax.functor._

trait GroupId

class GroupService[F[_]](implicit DB: DB[F]) {

  def createGroup(name: String): F[Group] =
    for {
      group <- DB.delay(new Group(name))
      _ <- DB.persist(group)
    } yield group


}
