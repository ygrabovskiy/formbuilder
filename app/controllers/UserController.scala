package controllers

import play.api.mvc.Action
import play.api._
import play.api.mvc._
                         import play.api.data._
import play.api.data.Forms._
import models.{UserService, User}
import java.util.UUID

class UserController extends Controller{

  val userForm = Form(
  mapping(
    "name" -> text,
    "email" -> email,
    "dateOfBirth" -> date
  )(User.apply(UUID.randomUUID().toString, _, _, _))(u => Some((u.name, u.email, u.dateOfBirth)))
)



  def add = Action{
    Ok(views.html.addUser(userForm))
  }

  def submitAdd()  = Action { implicit rc =>
    userForm.bindFromRequest().fold(
       badForm => BadRequest("bad request; "  + badForm.errors),
       user => {
         UserService.save(user)
         Ok("Users:\n " +UserService.listAll.mkString(",\n<br/>"))
       }
    )

  }

}
