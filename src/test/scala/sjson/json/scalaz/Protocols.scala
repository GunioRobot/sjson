package sjson
package json.scalaz

import scalaz._
import Scalaz._

import DefaultProtocol._
import JsonSerialization._
import dispatch.json._

object Protocols {
  case class Address(no: Int, street: String, city: String, zip: String)
  case class Person(lastName: String, firstName: String, age: Int, address: Address)
  case class Contact(lastName: String, firstName: String, address: Address, officeCity: String, officeAddress: Address)

  object AddressProtocol extends DefaultProtocol {

    implicit object AddressFormat extends Format[Address] {
      def reads(json: JsValue): ValidationNEL[String, Address] = json match {
        case m@JsObject(_) => 
          (field[Int]("no", m) |@| field[String]("street", m) |@| field[String]("city", m) |@| field[String]("zip", m)) { Address }

        case _ => "JsObject expected".fail.liftFailNel
      }

      def writes(p: Address) =
        List(
          tojson("no") <|*|> tojson(p.no), 
          tojson("street") <|*|> tojson(p.street), 
          tojson("city") <|*|> tojson(p.city), 
          tojson("zip") <|*|> tojson(p.zip)
        ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
          case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
          case Failure(errs) => errs.fail
        }
    }
  }

  object PersonProtocol extends DefaultProtocol {
    import AddressProtocol._

    implicit object PersonFormat extends Format[Person] {
      def reads(json: JsValue): ValidationNEL[String, Person] = json match {
        case m@JsObject(_) => 
          (field[String]("lastName", m) |@| field[String]("firstName", m) |@| field[Int]("age", m) |@| field[Address]("address", m)) { Person }

        case _ => "JsObject expected".fail.liftFailNel
      }
      def writes(p: Person) =
        List(
          tojson("lastName") <|*|> tojson(p.lastName), 
          tojson("firstName") <|*|> tojson(p.firstName), 
          tojson("age") <|*|> tojson(p.age), 
          tojson("address") <|*|> tojson(p.address)
        ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
          case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
          case Failure(errs) => errs.fail
        }
    }
  }

  object IncorrectPersonProtocol extends DefaultProtocol {
    import AddressProtocol._

    implicit object PersonFormat extends Format[Person] {
      def reads(json: JsValue): ValidationNEL[String, Person] = json match {
        case m@JsObject(_) => 
          (field[String]("LastName", m) |@| field[String]("firstname", m) |@| field[Int]("age", m) |@| field[Address]("address", m)) { Person }

        case _ => "JsObject expected".fail.liftFailNel
      }
      def writes(p: Person) =
        List(
          tojson("lastName") <|*|> tojson(p.lastName), 
          tojson("firstName") <|*|> tojson(p.firstName), 
          tojson("age") <|*|> tojson(p.age), 
          tojson("address") <|*|> tojson(p.address)
        ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
          case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
          case Failure(errs) => errs.fail
        }
    }
  }

  object IncorrectPersonProtocol1 extends DefaultProtocol {
    import AddressProtocol._

    implicit object PersonFormat extends Format[Person] {
      def reads(json: JsValue): ValidationNEL[String, Person] = json match {
        case m@JsObject(_) => 
          (field[String]("LastName", m) |@| field[String]("firstname", m) |@| field[Int]("age", m) |@| field[Address]("address", m)) { Person }

        case _ => "JsObject expected".fail.liftFailNel
      }
      def writes(p: Person) =
        List(
          tojson("lastname") <|*|> tojson(p.lastName), 
          tojson("firstname") <|*|> tojson(p.firstName), 
          tojson("age") <|*|> tojson(p.age), 
          tojson("addres") <|*|> tojson(p.address)
        ).sequence[({type λ[α]=ValidationNEL[String, α]})#λ, (JsValue, JsValue)] match {
          case Success(kvs) => JsObject(kvs.map{case (key, value) => (key.asInstanceOf[JsString], value)}).success
          case Failure(errs) => errs.fail
        }
    }
  }

  case class Shop(store: String, item: String, price: Int)
  implicit val ShopFormat: Format[Shop] = 
    asProduct3("store", "item", "price")(Shop)(Shop.unapply(_).get)

  import AddressProtocol._
  case class AddressBook(name: String, addresses: List[Address])
  implicit val AddressBookFormat: Format[AddressBook] = 
    asProduct2("name", "addresses")(AddressBook)(AddressBook.unapply(_).get)

  case class Base(no: String, name: String, addresses: List[Address])
  implicit val BaseFormat: Format[Base] = 
    asProduct3("no", "name", "addresses")(Base)(Base.unapply(_).get)

  class Derived(no: String, name: String, addresses: List[Address], special: Boolean) 
    extends Base(no, name, addresses) {
    val specialFlag = special
  }
  object DerivedProtocol extends DefaultProtocol {
    import dispatch.json._
    import JsonSerialization._
    implicit object DerivedFormat extends Format[Derived] {
      def reads(json: JsValue): Validation[NonEmptyList[String], Derived] = {
        val Success(b) = fromjson[Base](json)
        json match {
          case m@JsObject(_) =>
            val Success(spl) = field[Boolean]("specialFlag", m)
            new Derived(b.no, b.name, b.addresses, spl).success
          case _ => "JsObject expected".fail.liftFailNel
        }
      }
      def writes(a: Derived) = {
        val Success(o) = tojson(a: Base)
        val JsObject(m) = o
        JsObject(m ++ List((JsString("specialFlag"), tojson(a.specialFlag).toOption.get))).success
      }
    }
  }

  import dispatch.json._
  trait HttpType
  implicit val HttpTypeFormat: Format[HttpType] = new Format[HttpType] {
    def reads(json: JsValue): Validation[NonEmptyList[String], HttpType] = json match {
      case JsString("Get") => Get.success
      case JsString("Post") => Post.success
      case _ => "Invalid HttpType".fail.liftFailNel
    }
    def writes(a: HttpType) = a match {
      case Get => JsString("Get").success
      case Post => JsString("Post").success
    }
  }

  case object Get extends HttpType
  case object Post extends HttpType

  case class Http(url: String, t: HttpType)
  implicit val HttpFormat: Format[Http] = 
    asProduct2("url", "t")(Http)(Http.unapply(_).get)

  case class Bar(name: String, list: Option[List[Foo]])
  case class Foo(name: String, list: List[Bar])
  implicit val BarFormat: Format[Bar] = lazyFormat(asProduct2("name", "list")(Bar)(Bar.unapply(_).get))
  implicit val FooFormat: Format[Foo] = lazyFormat(asProduct2("name", "list")(Foo)(Foo.unapply(_).get))

  case class Name(name: String)
  implicit val NameFormat: Format[Name] = wrap[Name, String]("name")(_.name, Name)

  case class Holder(item: List[String])
  implicit val HolderFormat: Format[Holder] = wrap[Holder, List[String]]("item")(_.item, Holder)

  case class Account(no: String, name: String, addresses: Array[Address])
  implicit val AccountFormat: Format[Account] = 
    asProduct3("no", "name", "addresses")(Account)(Account.unapply(_).get)

  import sjson.json.TestBeans._
  implicit val AddressWithOptionalCityFormat: Format[AddressWithOptionalCity] =
    asProduct3("street", "city", "zip")(AddressWithOptionalCity)(AddressWithOptionalCity.unapply(_).get)
}
