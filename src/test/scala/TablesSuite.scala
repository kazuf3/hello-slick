import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import slick.jdbc.H2Profile.api._
import slick.jdbc.meta._
import scala.concurrent.ExecutionContext.Implicits.global

class TablesSuite extends funsuite.AnyFunSuite with BeforeAndAfter with ScalaFutures {
  implicit override val patienceConfig = PatienceConfig(timeout = Span(5, Seconds))

  val suppliers = TableQuery[Suppliers]
  val coffees = TableQuery[Coffees]
  
  var db: Database = _

  def createSchema() =
    db.run((suppliers.schema ++ coffees.schema).create).futureValue
  
  def insertSupplier() = {
    val suppliersInsert: DBIO[Unit] = DBIO.seq(
      suppliers += (101, "Acme, Inc.", "99 Market Street", "Groundsville", "CA", "95199"),
      suppliers += ( 49, "Superior Coffee", "1 Party Place", "Mendocino", "CA", "95460"),
      suppliers += (150, "The High Ground", "100 Coffee Lane", "Meadows", "CA", "93966")
    )
    db.run(suppliersInsert).futureValue
  }

  def insertCoffees() = {
    val coffeesInsert: DBIO[Option[Int]] = coffees ++= Seq (
        ("Colombian",         101, 7.99, 0, 0),
        ("French_Roast",       49, 8.99, 0, 0),
        ("Espresso",          150, 9.99, 10, 0),    //salesの値を10に変更
        ("Colombian_Decaf",   101, 8.99, 0, 0),
        ("French_Roast_Decaf", 49, 9.99, 10, 0)     //salesの値を10に変更
    )
    db.run(coffeesInsert).futureValue
  }

  before { db = Database.forConfig("h2mem1") }
  
  //更新情報入力
  val updateCoffeeName = "Colombian"
  val updateCoffeeSales = 10
  
  //Coffeesテーブル更新関数
  def UpdateCoffeeTable() = {
    db.run(coffees.filter(_.name === updateCoffeeName).map(_.sales).update(updateCoffeeSales)).futureValue
  }

  //補充対象仕入先・コーヒー名・個数表示関数
  def Replenishment() = {
    val supplierResult = db.run(coffees.filter(_.sales > 0).map(_.supID).result).futureValue    //売上があったコーヒーの仕入先ID抽出
    val coffeeNameResult = db.run(coffees.filter(_.sales > 0).map(_.name).result).futureValue   //売上があったコーヒー仕入先名
    val Quantity = db.run(coffees.filter(_.sales > 0).map(_.sales).result).futureValue          //補充数＝売り上げ数
    val num = supplierResult.size
    println("\n---------------Replenishment list---------------------")                         //表示
    for(i <- 0 to num-1 ){
      var supName = db.run(suppliers.filter(_.id === supplierResult(i)).map(_.name).result).futureValue
      println(" Supplier Name: " +supName.head+ ", Coffee Name: " +coffeeNameResult(i)+ ", Quantity: "+ Quantity(i))
    }
    println("------------------------------------------------------\n")
  }

 //Coffeesテーブル更新関数テスト
 test("Coffees table update works") {
    createSchema()
    insertSupplier()
    insertCoffees()
    UpdateCoffeeTable()
    val Result = db.run(coffees.filter(_.name === updateCoffeeName).map(_.sales).result).futureValue
    assert(Result.head == updateCoffeeSales)    //更新したいコーヒー名のsalesに更新したい数値が入っていることを確認
  }
  
  //補充対象仕入先・コーヒー名・個数表示関数
  test("Replenishment works") {
    createSchema()
    insertSupplier()
    insertCoffees()
    Replenishment()
    val supplierResult = db.run(coffees.filter(_.sales > 0).map(_.supID).result).futureValue
    val coffeeNameResult = db.run(coffees.filter(_.sales > 0).map(_.name).result).futureValue
    val Quantity = db.run(coffees.filter(_.sales > 0).map(_.sales).result).futureValue
    val num = supplierResult.size
    val supName = for {
      c <- coffees if c.sales > 0
      s <- suppliers if c.supID === s.id  
    }yield (s.name)
    val supNameResult = db.run(supName.result).futureValue
    assert(supplierResult == Vector(150, 49))                               //売上のあったサプライヤーIDが正しく抽出できていること
    assert(supNameResult == Vector("The High Ground", "Superior Coffee"))   //補充仕入先が正しく抽出できているか
    assert(coffeeNameResult == Vector("Espresso", "French_Roast_Decaf"))    //補充コーヒー名が正しいか
    assert(Quantity == Vector(10, 10))                                      //補充数量が正しいか
  }

  test("Creating the Schema works") {
    createSchema()
    
    val tables = db.run(MTable.getTables).futureValue

    assert(tables.size == 2)
    assert(tables.count(_.name.name.equalsIgnoreCase("suppliers")) == 1)
    assert(tables.count(_.name.name.equalsIgnoreCase("coffees")) == 1)
  }

  test("Inserting a Supplier works") {
    createSchema()
    insertSupplier()
    val insertCount = db.run(suppliers.result).futureValue
    assert(insertCount.size == 3)
  }
  
  test("Query Suppliers works") {
    createSchema()
    insertSupplier()
    val results = db.run(suppliers.result).futureValue
    assert(results.size == 3)
    assert(results.head._1 == 49)
  }
  
  after { db.close }
}
