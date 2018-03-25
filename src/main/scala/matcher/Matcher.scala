package matcher

import java.nio.file.{Files, Path, Paths}

import dao.{Client, ClientParser, Order, OrderParser}

import scala.collection.mutable

class Matcher {

  private val clients = mutable.Map[String, Client]()
  private val ordersQ = scala.collection.mutable.Queue[Order]()

  def loadClientsFromFile(path: String): Unit = {
    clients.clear()
    clients ++= ClientParser.loadClientsFromFile(path).map(cl => (cl.name, cl))
  }

  def loadOrdersFromFile(path: String): Unit = {
    ordersQ.clear()
    ordersQ ++= OrderParser.loadOrdersFromFile(path)
    calculateSums()
  }

  def findCounterOrder(order: Order): Option[Order] = {
    ordersQ.dequeueFirst(ord =>
      ord.clientName != order.clientName &&
        ord.count == order.count &&
        ord.price == order.price &&
        (ord.op == (if (order.op == "b") "s" else "b")))
  }


  private def calculateSums(): Unit = {
    ordersQ.dequeueAll{x =>
      println(s"Looking for pair for $x")
      findCounterOrder(x) match {
        case Some(y) =>
          println(s"Found pair $y")
          updateClients(x, y)
          true
        case None => false
      }
    }
  }

  private def updateClients(newOrder: Order, counterOrder: Order): Unit = {
    clients += newOrder.clientName -> clients(newOrder.clientName).applyOrder(newOrder)
    clients += counterOrder.clientName -> clients(counterOrder.clientName).applyOrder(counterOrder)
  }

  def addNewOrders(newOrders: Seq[Order]): Unit = newOrders.foreach(addNewOrder)

  def addNewOrder(newOrder: Order): Unit = {
    ordersQ += newOrder
    findCounterOrder(newOrder) match {
      case Some(counterOrder) =>
        updateClients(newOrder, counterOrder)
        ordersQ.dequeueFirst(_ == newOrder)
      case None =>
    }
  }

  def addNewClients(newClients: Seq[Client]): Unit = newClients.foreach(addNewClient)

  def addNewClient(newClient: Client): Unit = {
    clients.get(newClient.name) match {
      case Some(existingClient) => throw new Exception(s"Sorry, client with name${existingClient.name} already exist")
      case None => clients += (newClient.name ->  newClient)
    }
  }

  def getAllClients: Map[String, Client] = clients.toMap

  def getUnApplyiedOrders: Seq[Order] = ordersQ

  def saveClientsToFile(file: String): Path = {
    val path = Paths.get(file)
    val clientsString = (for (client <- clients)
      yield client._2.toString())
    .mkString("\n")
    Files.write(path, clientsString.getBytes())
  }
}
