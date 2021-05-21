import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import slick.basic.DatabasePublisher
import slick.jdbc.H2Profile.api._

object Exercises {
  val suppliers: TableQuery[Suppliers] = TableQuery[Suppliers]
  val coffees: TableQuery[Coffees] = TableQuery[Coffees]

  // Exercise 2
  def new_sale(name:String, number:Int)(implicit db: Database) = {
    val getOldSales = for {
      c <- coffees if c.name === name
    } yield c.sales
    // wait at here because we need the resulting value
    val old:Seq[Int] = Await.result(db.run(getOldSales.result), Duration.Inf)
    val new_sales:Int = old.head+number
    val updateSales = coffees.filter(_.name===name).map(_.sales).update(new_sales)
    Await.result(db.run(coffees.filter(_.name===name).map(_.sales).update(new_sales)), Duration.Inf)
  }

  // Exercise 3
  def getResupplies()(implicit db: Database): Seq[(String, String, Int)] = {
    val joinAction = for{
      coffee <- coffees if coffee.sales > 0
      supplier <- suppliers if supplier.id === coffee.supID
    } yield (supplier.name, coffee.name, coffee.sales)
    Await.result(db.run(joinAction.result), Duration.Inf)
  }
  def showResupplies()(implicit db: Database): Unit = {
    println("仕入れ元\tコーヒー名\t数量")
    getResupplies()(db).foreach( joined => println( joined.productIterator.mkString("\t")) )
  }
}
