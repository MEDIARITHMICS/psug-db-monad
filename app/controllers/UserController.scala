package controllers


import cats.{ApplicativeError, Monad}
import cats.effect.Async
import cats.syntax.functor._
import com.mediarithmics._
import com.mediarithmics.tag.TLong
import javax.inject._
import play.api.mvc._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import tag.LongConverter
import cats.syntax.either._
import cats.syntax.functor._
import com.mediarithmics.iodb._
import javax.persistence.EntityManager

//import com.mediarithmics.common._
//object User{
//  case class Shape[F[_]](id : F[TLong[UserId]], name: String)
//  type CreateRequest = Shape[Empty]
//  type Resource = Shape[Id]
//
////  import io.circe.generic.semiauto._
////  implicit val createRequestDecoder = deriveDecoder[Shape[Empty]]
//}
//
//object Group {
//  case class Shape[F[_]](id : F[TLong[GroupId]], name: String)
//  type CreateRequest = Shape[Empty]
//  type Resource = Shape[Id]
//}


object User {
  case class CreateRequest(name: String)
  case class Resource(id : TLong[UserId], name: String)
}

object Group {
  case class CreateRequest(name: String)
  case class Resource(id : TLong[GroupId], name: String)
}

object ControllerUtils {

  implicit def tlongEncoder[T]: Encoder[TLong[T]] =  Encoder[Long].asInstanceOf[Encoder[TLong[T]]]
  implicit def tlongDecoder[T]: Decoder[TLong[T]] =  Decoder[Long].asInstanceOf[Decoder[TLong[T]]]

  implicit class ParseOp(val request : Request[AnyContent]) extends AnyVal {

    def bodyAs[T : Decoder]: Either[Error, T] =  for {
      content <- request.body.asText.toRight[Error](
        DecodingFailure("Cannot extract string from http request body", Nil))
      value <- decode[T](content)
    } yield value

  }


}

class UserControllerOps[F[_]: Monad, EM](userService: UserService[F, EM],
                                          groupService: GroupService[F, EM]) {
  def createUser(request: User.CreateRequest) : F[User.Resource] =
    userService.createUser(request.name).map(created => User.Resource(created.getId.tag, created.getName))

  def createGroup(request: Group.CreateRequest) : F[Group.Resource]=
    groupService.createGroup(request.name).map(created => Group.Resource(created.getId.tag, created.getName))

  def addUserToGroup(userId: TLong[UserId], groupId: TLong[GroupId]): F[Unit] =
    userService.addUserToGroup(userId.tag[UserId], groupId)

  def getUserGroups(userId: TLong[UserId]) : F[List[Group.Resource]] =
    for {
      groups <- userService.getUserGroups(userId)
    } yield groups.map(g => Group.Resource(g.getId.tag, g.getName))
}

import ControllerUtils._
@Singleton
class UserController @Inject()(cc: ControllerComponents,
                               userService: UserService[TransactionableIO, EntityManager],
                               groupService: GroupService[TransactionableIO, EntityManager],
                               Async : Async[TransactionableIO],
                              )
  extends AbstractController(cc) {

  implicit val A : ApplicativeError[TransactionableIO, Throwable] = Async

  val ops = new UserControllerOps(userService, groupService)(Async)

  def createUser(): Action[AnyContent] = Action.async { r : Request[AnyContent] =>
    val doCreate = for {
      userRequestE <- Async.delay(r.bodyAs[User.CreateRequest])
      userRequest <- userRequestE.liftTo[TransactionableIO]
      response <- ops.createUser(userRequest)
    } yield Ok( response.asJson.noSpaces )

    doCreate.transact.unsafeToFuture()
  }

  def createGroup(): Action[AnyContent] = Action.async {   r : Request[AnyContent] =>
    val doCreate = for {
      groupRequestE <- Async.delay(r.bodyAs[Group.CreateRequest])
      groupRequest <- groupRequestE.liftTo[TransactionableIO]
      response <- ops.createGroup(groupRequest)
    } yield Ok( response.asJson.noSpaces )

    doCreate.transact.unsafeToFuture()
  }

  def addUserToGroup(userId: Long, groupId:Long): Action[AnyContent] = Action.async {  _ : Request[AnyContent] =>
    ops
      .addUserToGroup(userId.tag[UserId], groupId.tag[GroupId])
      .as(Ok(""))
      .transact.unsafeToFuture()
  }

  def getUserGroups(userId: Long): Action[AnyContent] = Action.async {  _ : Request[AnyContent] =>

    val doGet= for {
      resources <- ops.getUserGroups(userId.tag[UserId])
    } yield Ok(resources.asJson.noSpaces)

    doGet.transact.unsafeToFuture()
  }

}
