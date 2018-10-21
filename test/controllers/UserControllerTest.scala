package controllers

import cats._
import cats.syntax.functor._
import com.mediarithmics.DB.{EntityManager, Transactioner}
import com.mediarithmics.{DB, EntityWithId, GroupService, UserService, Group => GroupEntity, User => UserEntity}
import org.scalatest.FunSuite

import scala.collection.mutable
import scala.reflect.ClassTag


case class TestContext(users: mutable.Map[Long, UserEntity], groups: mutable.Map[Long, GroupEntity])

object TestContext {

  implicit def EntityManager = new EntityManager[Id, TestContext] {

    var id: Long = 0


    override def findById[A: ClassTag](em: TestContext, id: Long): Id[A] = {
      val clazz = implicitly[ClassTag[A]].runtimeClass
      clazz.getSimpleName match {
        case "User" => em.users(id).asInstanceOf[A]
        case "Group" => em.groups(id).asInstanceOf[A]
        case n => throw new Exception(s"unknown entity $n")
      }
    }

    override def persist(em: TestContext, a: Any): Id[Unit] = {
      a match {
        case u: UserEntity =>
          if (u.getId == 0) {
            id += 1
            u.setId(id)
          }
          em.users += (u.getId -> u)
        case g: GroupEntity =>
          if (g.getId == 0) {
            id += 1
            g.setId(id)
          }

          em.groups += (g.getId -> g)
      }
    }

    override def merge[A](em: TestContext, a: A): Id[A] = persist(em, a).as(a)

    override def remove[A <: EntityWithId : ClassTag](em: TestContext, e: A): Id[Unit] =
      e match {
        case u: UserEntity => em.users -= u.getId
        case g: GroupEntity => em.groups -= g.getId
      }

  }

  implicit def Transactioner = new Transactioner[Id, TestContext] {
    val context = new TestContext(mutable.Map.empty, mutable.Map.empty)

    override def transactionally[A](fa: TestContext => Id[A]): Id[A] = fa(context)
  }


  def db: DB[Id, TestContext] = new DB

}


class UserControllerTest
  extends FunSuite {

  implicit val DB = TestContext.db

  val userService = new UserService[Id, TestContext]()
  val groupService = new GroupService[Id, TestContext]()

  val controllerOps = new UserControllerOps(userService, groupService)


  test("create user") {
    val user = User.CreateRequest("Toto")

    val createdUser = controllerOps.createUser(user)
    assert(createdUser.name == user.name)
    assert(createdUser.id != null)

  }


  test("create group") {
    val group = Group.CreateRequest("psug")

    val createdGroup = controllerOps.createGroup(group)
    assert(createdGroup.name == group.name)
    assert(createdGroup.id != null)

  }


  test("add user to group") {

    val user = User.CreateRequest("Joe")
    val createdUser = controllerOps.createUser(user)


    val groupName = "lsug"
    val group = Group.CreateRequest(groupName)

    val createdGroup = controllerOps.createGroup(group)

    controllerOps.addUserToGroup(createdUser.id, createdGroup.id)

    val result = controllerOps.getUserGroups(createdUser.id)

    assert(result.size == 1)
    assert(result.head.name == groupName)
  }


}
