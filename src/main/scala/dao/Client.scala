package dao

import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

case class Client(name: String,
                  dollars: Int,
                  papers: Map[String, Int]) {

  def applyOrder(order: Order): Client =
    if (order.clientName != this.name) throw new Exception(s"Order $order is not associated with $this") else
      this.copy(dollars = this.dollars + order.cost,
        papers = this.papers + (order.name -> (this.papers(order.name) + order.signedCount)))

  private def sep = "\t"

  override def toString: String = (List(name, dollars) ::: papers.values.toList).mkString(sep)
}


object ClientParser {
  def loadClientsFromFile(path: String): Seq[Client] = {
    val lines = Files.readAllLines(Paths.get(path)).asScala
    lines.map(x => x.split("\t").toList match {
      case clientName :: dollars :: a :: b :: c :: d :: Nil =>
        Client(clientName, Integer.valueOf(dollars),
          Map("A" -> Integer.valueOf(a), "B" -> Integer.valueOf(b),
            "C" -> Integer.valueOf(c), "D" -> Integer.valueOf(d)))
    })
  }
}
