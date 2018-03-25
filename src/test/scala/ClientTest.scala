import dao.{Client, ClientParser, Order}
import org.scalatest.FlatSpec

class ClientTest extends FlatSpec{
  "File" should "parse into list of clients" in {
    val path = "data/clients.txt"
    val clients = ClientParser.loadClientsFromFile(path)
    assert(clients.nonEmpty)
  }

  "Client" should "correctly transform into string" in {
    val papers = Map("A"->1, "B"-> 2 , "C" -> 3, "D" -> 4)
    val client = Client("Alex", 102, papers)
    assert(client.toString == "Alex\t102\t1\t2\t3\t4")
  }

  "Client" should "correctly change state after applying order 1" in {
    val order = Order("Alex", "s", "A", 5, 4)
    val client = Client("Alex", 20, Map("A" -> 7))
    val clientAfterOrder = client.applyOrder(order)
    assert(clientAfterOrder.papers("A") == 3)
    assert(clientAfterOrder.dollars == 40)
  }

  "Client" should "correctly change state after applying order 2" in {
    val order = Order("Alex", "b", "A", 5, 4)
    val client = Client("Alex", 20, Map("A" -> 7))
    val clientAfterOrder = client.applyOrder(order)
    assert(clientAfterOrder.papers("A") == 11)
    assert(clientAfterOrder.dollars == 0)
  }

  "Client" should "raise exception when wrong order is applied" in {
    val order = Order("Alex", "b", "A", 5, 4)
    val client = Client("Client", 20, Map("A" -> 7))
    assertThrows[Exception]{client.applyOrder(order)}

  }
}
