package com.mediarithmics

import cats.Functor
import cats.syntax.functor._

trait GroupId

class GroupService[F[_], EM](implicit DB: DB[F, EM], F: Functor[F]) {

  def createGroup(name: String): F[Group] =
    DB.transactionally {
      val group = new Group(name)
      DB.persist(group).as(group)
    }


}
