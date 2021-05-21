import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import slick.jdbc.H2Profile.api._
import slick.jdbc.meta._

class TablesSuite extends funsuite.AnyFunSuite with BeforeAndAfter with ScalaFutures {
  implicit override val patienceConfig = PatienceConfig(timeout = Span(5, Seconds))

  val suppliers = TableQuery[Suppliers]
  val coffees = TableQuery[Coffees]

  implicit var db: Database = _

  def createSchema() =
    db.run((suppliers.schema ++ coffees.schema).create).futureValue

  val suppliers_list = Seq(
    (101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
    ( 49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
    (150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
  )
  def insertSupplier() = db.run(suppliers ++= suppliers_list).futureValue

  before { db = Database.forConfig("h2mem1") }

  test("Creating the Schema works") {
    createSchema()

    val tables = db.run(MTable.getTables).futureValue

    assert(tables.size == 2)
    assert(tables.count(_.name.name.equalsIgnoreCase("suppliers")) == 1)
    assert(tables.count(_.name.name.equalsIgnoreCase("coffees")) == 1)
  }

  test("Inserting a Supplier works") {
    createSchema()

    val insertCount = insertSupplier()
    assert(insertCount == Some(3))
  }

  test("Query Suppliers works") {
    createSchema()
    insertSupplier()
    val results = db.run(suppliers.result).futureValue
    assert(results.size == 3)
    assert(Set(49, 101, 150).contains(results.head._1))
  }

  def insertCoffees() = {
    val coffees_list = Seq(
      ("Colombian",         101, 7.99, 0, 0),
      ("French_Roast",      101, 8.99, 0, 0),
      ("Espresso",          101, 9.99, 0, 0),
      ("Colombian_Decaf",   101, 8.99, 0, 0),
      ("French_Roast_Decaf",101, 9.99, 0, 0)
    )
    db.run(coffees ++= coffees_list).futureValue
  }
  test("New sales function works") {
    createSchema()
    insertSupplier()
    insertCoffees()
    Exercises.new_sale("Colombian", 5)
    val result = db.run(coffees.filter(_.name==="Colombian").map(_.sales).result).futureValue.head
    assert(result == 5)
    Exercises.new_sale("Colombian", 105)
    val result2 = db.run(coffees.filter(_.name==="Colombian").map(_.sales).result).futureValue.head
    assert(result2 == 110)
  }
  test("New sales on non-existent item fails"){
    createSchema()
    assertThrows[java.lang.UnsupportedOperationException]{
      Exercises.new_sale("Colombian", 5)
    }
  }
  test("Resupply list is correct"){
    createSchema()
    insertSupplier()
    insertCoffees()
    Exercises.new_sale("Colombian", 100)
    Exercises.new_sale("Espresso", 200)
    val result = Exercises.getResupplies()
    Exercises.showResupplies()
    assert(result.to[Set] == Set(("Acme, Inc.", "Espresso", 200), ("Acme, Inc.", "Colombian", 100)))
  }
  after { db.close }
}

