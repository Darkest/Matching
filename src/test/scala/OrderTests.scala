import dao.{Order, OrderParser}
import org.scalatest.FlatSpec

class OrderTests extends FlatSpec{

 "File" should "parse into list of Orders" in {
   val path = "data/orders.txt"
   val list = OrderParser.loadOrdersFromFile(path)
   assert(list.nonEmpty)
 }

  "Order" should "correctly count its cost for sell" in {
    val order = Order("Alex", "s", "A", 5, 4)
    assert(order.cost == 20)
  }

  "Order" should "correctly count its cost for buy" in {
    val order = Order("Alex", "b", "A", 5, 4)
    assert(order.cost == -20)
  }

  "Order" should "correctly return signedCount for sell op" in {
    val order = Order("Alex", "s", "A", 5, 4)
    assert(order.signedCount == -4)
  }

  "Order" should "correctly return signedCount for buy op" in {
    val order = Order("Alex", "b", "A", 5, 4)
    assert(order.signedCount == 4)
  }
}
