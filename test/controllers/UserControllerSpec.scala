package controllers


import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{EitherValues, FunSuite}
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test.Helpers._
import play.api.test._
import play.mvc.Http.HeaderNames
import ControllerUtils._
import com.mediarithmics.CustomModule
import play.api.inject.guice.GuiceApplicationBuilder


/**
  * Add your spec here.
  * You can mock out a whole application including requests, plugins etc.
  *
  * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
  */
class UserControllerSpec
  extends FunSuite
    with EitherValues
    with WsScalaTestClient
    with GuiceOneAppPerSuite
    with Injecting {

  override lazy val app =  GuiceApplicationBuilder().bindings(new CustomModule()).build()

  val headers = FakeHeaders(Seq(HeaderNames.HOST -> "localhost"))


  test("request parsing") {

    decode[User.CreateRequest]("""{"name":"Toto"}""") match {
      case Left(err) =>
        println(err.getMessage)
        fail(err)
      case Right(user) => assert(user === User.CreateRequest("Toto"))

    }

  }

  test("create user") {
    val user = User.CreateRequest("Toto")
    val request = FakeRequest(POST, "/users", headers, user.asJson.noSpaces)

    val answer = route(app, request).get

    assert(status(answer) == OK)
    val createdUser = decode[User.Resource](contentAsString(answer)).right.value
    assert(createdUser.name == user.name )
    assert(createdUser.id != null)

  }


  test("create group") {
    val group = Group.CreateRequest("psug")
    val request = FakeRequest(POST, "/groups", headers, group.asJson.noSpaces)

    val answer = route(app, request).get

    assert(status(answer) == OK)
    val createdUser = decode[Group.Resource](contentAsString(answer)).right.value
    assert(createdUser.name == group.name )
    assert(createdUser.id != null)

  }



  test("add user to group") {

    val user = User.CreateRequest("Joe")
    val userRequest = FakeRequest(POST, "/users", headers, user.asJson.noSpaces)
    val userAnswer = route(app, userRequest).get
    val createdUser = decode[User.Resource](contentAsString(userAnswer)).right.value

    val groupName = "lsug"
    val group = Group.CreateRequest(groupName)
    val createGroupRequest =
      FakeRequest(POST, "/groups", headers, group.asJson.noSpaces)
    val groupAnswer = route(app, createGroupRequest).get
    val createdGroup = decode[Group.Resource](contentAsString(groupAnswer)).right.value

    val addToGroupRequest = FakeRequest(POST, s"/user/${createdUser.id}/group/${createdGroup.id}",
      headers, group.asJson.noSpaces)
    val addToGroupAnswer = route(app, addToGroupRequest).get

    assert(status(addToGroupAnswer) == OK)

    val getGroups = FakeRequest(GET, s"/user/${createdUser.id}/groups")
    val getGroupsAnswer = route(app, getGroups).get

    val result = decode[List[Group.Resource]](contentAsString(getGroupsAnswer)).right.value

    assert(result.size == 1)
    assert(result.head.name == groupName)
  }


}
