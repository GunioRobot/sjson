package sjson
package json.scalaz

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TypeclassSerializerSpec extends Spec with ShouldMatchers {

  import DefaultProtocol._
  import JsonSerialization._
  import scalaz._
  import Scalaz._

  describe("Serialization using Person protocol") {
    it ("should serialize a Person") {
      import Protocols._
      import PersonProtocol._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      val p = Person("ghosh", "debasish", 20, a)
      tojson(p) map fromjson[Person] should equal(p.success.success)
    }
  }

  describe("Serialization using incorrect Person protocol") {
    it ("serialization should fail") {
      import Protocols._
      import IncorrectPersonProtocol._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      val p = Person("ghosh", "debasish", 20, a)
      val Success(Failure(l)) = tojson(p) map fromjson[Person]
      l.list should equal (List("field LastName not found", "field firstname not found"))
    }
  }

  describe("Serialization using incorrect Person protocol1") {
    it ("serialization should fail") {
      import Protocols._
      import IncorrectPersonProtocol1._
      val a = Address(12, "Monroe Street", "Denver", "80231")
      val p = Person("ghosh", "debasish", 20, a)
      println(tojson(p))
      println(tojson(p) map fromjson[Person])
      // val Success(Failure(l)) = tojson(p) map fromjson[Person]
      // l.list should equal (List("field LastName not found", "field firstname not found"))
    }
  }

  describe("Serialization of lists") {
    it ("should serialize list of Ints") {
      val l1 = List(100, 200, 300, 400)
      tojson(l1) map fromjson[List[Int]] should equal(l1.success.success)
    }

    it ("should serialize list of Strings") {
      val l2 = List("dg", "mc", "rc", "nd")
      tojson(l2) map fromjson[List[String]] should equal(l2.success.success)
    }
  }
  val jsonString = 
    """{
         "lastName" : "ghosh", 
         "firstName" : "debasish", 
         "age" : 20, 
         "address" : { "no" : 12, "street" : "Monroe Street", "city" : "Denver", "zip" : "80231" }, 
         "phone" : { "no" : "3032144567", "ext" : 212 },
         "office" :
          {
            "name" : "anshinsoft",
            "address" : { "no" : 23, "street" : "Hampden Avenue", "city" : "Denver", "zip" : "80245" } 
          }
       }"""

  import Protocols._
  import AddressProtocol._
  import dispatch.json._
  import Js._

  val js = Js(jsonString)
  val c = Contact("ghosh","debasish",Address(12,"Monroe Street","Denver","80231"),"Denver",Address(23,"Hampden Avenue","Denver","80245"))

  describe("Serialization from arbitrary JSON string") {
    it ("should serialize into Contact") {

      (field[String]("lastName", js)    |@| 
        field[String]("firstName", js)   |@| 
        field[Address]("address", js)    |@| 
        field[String]("city", (('office ! obj) andThen ('address ? obj))(js)) |@|
        field[Address]((('office ! obj) andThen ('address ! obj)), js)) { Contact } should equal(c.success)
    }

    it ("should not serialize but report list of errors") {

      (field[String]("lastName", js)    |@| 
        field[String]("FirstName", js)   |@| 
        field[Address]("address", js)    |@| 
        field[String]("cty", (('office ! obj) andThen ('address ? obj))(js)) |@|
        field[Address]((('office ! obj) andThen ('address ! obj)), js)) { Contact }.fail.toOption.get.list should equal(List("field FirstName not found", "field cty not found"))
    }

    it("should serialize monadically") {
      // reader monad
      val contact =
        for {
          last    <- field_c[String]("lastName")
          first   <- field_c[String]("firstName")
          address <- field_c[Address]("address")
          office  <- field_c[Address]((('office ! obj) andThen ('address ! obj)))
        }
        yield(last |@| first |@| address |@| office)

      // city needs to be parsed separately since we are working on part of js
      val city = field_c[String]("city")

      // compose everything and build a Contact
      (contact(js) |@| city((('office ! obj) andThen ('address ? obj))(js))) { (last, first, address, office, city) => 
        Contact(last, first, address, city, office) } should equal(c.success)
    }
  }

  describe("Serialization of simple objects") {
    it("should serialize into json and back") {
      import Protocols._
      val shop = Shop("Shoppers Stop", "dress material", 1000)
      tojson(shop) map fromjson[Shop] should equal(shop.success.success)
    }
  }

  describe("Serialization of Maps") {
    it ("should serialize Map of Strings & Strings") {
      val m = Map("100" -> "dg", "200" -> "mc")
      tojson(m) map fromjson[Map[String, String]] should equal(m.success.success)
    }
  }

  describe("Serialization of composite objects") {
    it("should serialize into json and back") {
      import Protocols._
      val addressBook = AddressBook("Debasish Ghosh", 
        List(Address(100, "monroe st", "denver", "80231"), Address(23, "pine drive", "santa clara", "95054")))
      tojson(addressBook) map fromjson[AddressBook] should equal(addressBook.success.success)
    }
  }

  describe("Serialization of composite objects with arrays") {
    it("should serialize into json and back") {
      import Protocols._
      val account = Account("123", "Debasish Ghosh", 
        Array(Address(100, "monroe st", "denver", "80231"), Address(234, "pine drive", "santa clara", "95054")))

      val Success(Success(ac)) = (tojson(account) map fromjson[Account])
      ac.no should equal(account.no)
      ac.name should equal(account.name)
      ac.addresses should be === account.addresses
    }
  }

  describe("Serialization of Option") {
    it("should serialize an option field") {
      val str = Some("debasish")
      tojson[Option[String]](str) map fromjson[Option[String]] should equal(str.success.success)
      tojson[Option[String]](None) map fromjson[Option[String]] should equal(None.success.success)

      val i = Some(200)
      tojson[Option[Int]](i) map fromjson[Option[Int]] should equal(i.success.success)
    }
    it("should serialize AddressWithOptionalCity") {
      import sjson.json.TestBeans._
      import Protocols._
      val ad = AddressWithOptionalCity("garer math", Some("mumbai"), "400087")
      tojson(ad) map fromjson[AddressWithOptionalCity] should equal(ad.success.success)
    }
    it("should serialize AddressWithOptionalCity without city") {
      import sjson.json.TestBeans._
      import Protocols._
      val ad = AddressWithOptionalCity("garer math", None, "400087")
      tojson(ad) map fromjson[AddressWithOptionalCity] should equal(ad.success.success)
    }
  }

  describe("Serialization of tuples") {
    it("should serialize tuples of primitive types") {
      val t1 = ("debasish", 12)
      tojson(t1) map fromjson[Tuple2[String, Int]] should equal(t1.success.success)
      val t2 = ("debasish", 12, "jonas")
      tojson(t2) map fromjson[Tuple3[String, Int, String]] should equal(t2.success.success)
    }
    it("should serialize tuples of user defined types") {
      import Protocols._
      import AddressProtocol._
      val t1 = ("debasish", Address(102, "monroe st", "denver", "80231"))
      tojson[Tuple2[String, Address]](t1) map fromjson[Tuple2[String, Address]] should equal(t1.success.success)
    }
  }

  /**
  describe("Serialization of mutable sets") {
    it("should serialize mutable sets of primitive types") {
      import scala.collection._
      val s = mutable.Set(12, 13, 10, 23, 25)
      fromjson[mutable.Set[Int]](tojson(s)) should equal(s)
    }
    it("should serialize mutable sets of addresses") {
      import scala.collection._
      import Protocols._

      val s = mutable.Set(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231"))
      fromjson[mutable.Set[Address]](tojson(s)) should equal(s)
    }
    it("should serialize mutable sets of custom data types") {
      import scala.collection._
      import Protocols._

      val s = mutable.Set(
        ("debasish", Address("monroe st", "denver", "80231")), 
        ("maulindu", Address("tamarac st", "boulder", "80231")))
      fromjson[mutable.Set[(String, Address)]](tojson(s)) should equal(s)
    }
  }

  describe("Serialization of immutable sets") {
    it("should serialize immutable sets of primitive types") {
      import scala.collection._
      val s = immutable.Set(12, 13, 10, 23, 25)
      fromjson[immutable.Set[Int]](tojson(s)) should equal(s)
    }
    it("should serialize immutable sets of addresses") {
      import scala.collection._
      import Protocols._

      val s = immutable.Set(Address("monroe st", "denver", "80231"), Address("tamarac st", "boulder", "80231"))
      fromjson[immutable.Set[Address]](tojson(s)) should equal(s)
    }
    it("should serialize immutable sets of custom data types") {
      import scala.collection._
      import Protocols._

      val s = immutable.Set(
        ("debasish", Address("monroe st", "denver", "80231")), 
        ("maulindu", Address("tamarac st", "boulder", "80231")))
      fromjson[immutable.Set[(String, Address)]](tojson(s)) should equal(s)
    }
  }
  **/

  describe("Serialization of complex types") {
    it("should serialize complex types") {
      val l = List(Map("1"->"dg", "2"->"mc"), Map("1"->"irc", "2"->"rc", "3"->"nd"))
      tojson(l) map fromjson[List[Map[String, String]]] should equal(l.success.success)
    }
  }

  describe("Serialization of wrappers") {
    it("should serialize") {
      import Protocols._
      val n = Name("debasish ghosh")
      tojson(n) map fromjson[Name] should equal(n.success.success)
    }
    it("should serialize list wrappers") {
      import Protocols._
      val n = Holder(List("debasish ghosh", "jonas boner", "stephan schmidt"))
      tojson(n) map fromjson[Holder] should equal(n.success.success)
    }
  }

  describe("Serialization with inheritance") {
    it("should serialize") {
      import Protocols._
      import DerivedProtocol._
      val sa = new Derived("123", "debasish ghosh", List(Address(100, "monroe st", "denver", "80231"), Address(23, "tamarac st", "boulder", "80231")), true)
      val Success(acc) = tojson(sa) map fromjson[Derived]
      acc should equal(sa.success)
    }
  }

  describe("Serialization with case objects") {
    it("should serialize") {
      import Protocols._
      val h = Http("http://www.google.com", Get)
      val h1 = tojson(h) map fromjson[Http]
      h1 should equal(h.success.success)
    }
  }

  describe("Serialization of mutually recursive types") {
    it("should serialize without recursion") {
      import Protocols._
      val f = Foo("testFoo", List(Bar("test1", None), Bar("test2", None)))
      tojson(f) map fromjson[Foo] should equal(f.success.success)
    }
    it("should serialize with recursion") {
      import Protocols._
      val fBar = Foo("testFoo", List(Bar("test1", Some(List(Foo("barList", List(Bar("test", None), Bar("test2", None))))))))
      tojson(fBar) map fromjson[Foo] should equal(fBar.success.success)
    }
  }
}
