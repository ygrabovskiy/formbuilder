package models
 import anorm._
import java.util.Date
import play.api.db.DB
import scala.collection.mutable

case class User(id: String, name: String, email: String, dateOfBirth: Date) {

}

object UserService {
  val users = mutable.Map[String, User]()
  def save(user: User) = users.put(user.id, user)
  def get(id: String) = users.get(id)
  def listAll = users.values
}

