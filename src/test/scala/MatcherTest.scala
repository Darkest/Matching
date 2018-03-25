import dao.{Client, Order, OrderParser}
import matcher.Matcher
import org.scalatest.FlatSpec

class MatcherTest extends FlatSpec{

  def testClients: Seq[Client] = {
    val alexPapers = Map("A"->5, "B"-> 2 , "C" -> 3, "D" -> 4)
    val alex = Client("Alex", 102, alexPapers)
    val bobPapers = Map("A"->1, "B"-> 12 , "C" -> 13, "D" -> 14)
    val bob = Client("Bob", 100, bobPapers)
    val alicePapers = Map("A"->0, "B"-> 0 , "C" -> 3, "D" -> 0)
    val alice = Client("Alice", 100, alicePapers)

    Seq(alex, alice, bob)
  }

  "calculateSums" should "result to the same state when orders are added one by one" in {
    val batchMatcher =  new Matcher
    batchMatcher.loadClientsFromFile("data/clients.txt")
    batchMatcher.loadOrdersFromFile("data/orders.txt")

    val onebyoneMatcher = new Matcher
    onebyoneMatcher.loadClientsFromFile("data/clients.txt")
    onebyoneMatcher.addNewOrders(OrderParser.loadOrdersFromFile("data/orders.txt"))

    assert(batchMatcher.getUnApplyiedOrders == onebyoneMatcher.getUnApplyiedOrders)
    assert(batchMatcher.getAllClients == onebyoneMatcher.getAllClients)

  }

  "New orders" should "be added correctly and clients should be updated" in {

    val newSellOrder = Order("Alex", "s", "A", 3, 4)
    val newBuyOrder = Order("Bob", "b", "A", 3, 4)
    val matcher = new Matcher
    matcher.addNewClients(testClients)
    matcher.addNewOrders(Seq(newSellOrder, newBuyOrder))

    val updatedClients = matcher.getAllClients

    assert(updatedClients("Alex") == Client("Alex", 102 + 12, Map("A"->1, "B"-> 2 , "C" -> 3, "D" -> 4)))
    assert(updatedClients("Bob") == Client("Bob", 100 - 12, Map("A"->5, "B"-> 12 , "C" -> 13, "D" -> 14)))
    assert(updatedClients("Alice") == Client("Alice", 100, Map("A"->0, "B"-> 0 , "C" -> 3, "D" -> 0)))
    assert(matcher.getUnApplyiedOrders.isEmpty)
  }

  "saveClientsToFile" should "write clients to the file" in {
    val batchMatcher =  new Matcher
    batchMatcher.loadClientsFromFile("data/clients.txt")
    batchMatcher.loadOrdersFromFile("data/orders.txt")

    batchMatcher.saveClientsToFile("data/clientsOut.txt")
  }
}
