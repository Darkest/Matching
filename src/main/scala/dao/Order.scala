package dao

import java.nio.file.{Files, Paths}

import scala.collection.JavaConverters._
case class Order (clientName: String,
                  op: String,
                  name: String,
                  price: Int,
                  count: Int){
  def cost: Int = op match {
    case "s" => count * price
    case "b" => -(count * price)
  }

  def signedCount: Int = op match {
    case "s" => -count
    case "b" => count
  }
}


object OrderParser {
    def loadOrdersFromFile(path: String): Seq[Order] = {
      val lines = Files.readAllLines(Paths.get(path)).asScala
      lines.map(x => x.split("\t").toList match {
        case clientName :: op :: name :: price  :: count :: Nil=>
          Order(clientName, op, name, Integer.valueOf(price), Integer.valueOf(count))
      })
    }
  }

