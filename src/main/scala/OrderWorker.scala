import java.util.{Date, UUID}
import java.io.{BufferedWriter, File, FileInputStream, FileOutputStream, InputStream, OutputStreamWriter}


import models.{Client, OperationType, Order, SecuritiesType}

class OrderWorker {

  import scala.io.Source

  def readClients(fileName: String, enc: String): Seq[Client] = {
    readFile(fileName, enc)
      .map { line =>
        line.split("\t") match {
          // Искренне верим, что формат данных правельный и не обрабытвываем исключения
          case Array(user: String,
          money: String,
          bananceA: String,
          balanceB: String,
          balanceC: String,
          balanceD: String) =>
            Client(user, money.toInt, bananceA.toInt, balanceB.toInt, balanceC.toInt, balanceD.toInt)
        }
      }
  }

  def processOrder(fileName: String, enc: String, clients: Seq[Client]) = {

    val ordersLine = readFile(fileName, enc).toSeq

    val (currentOrder, orderLineNew) = getOrderFromLine(ordersLine)

    if (currentOrder.isDefined)
      makeOrder(currentOrder.get, clients, Seq(), orderLineNew)
  }


  private def makeOrder(order: Order,
                        clients: Seq[Client],
                        orders: Seq[Order],
                        ordersLine: Seq[String]): Unit = {
    val workOrder = orders.filterNot(_.name == order.name)
      .find(o => compareOrders(order, o))

    val (currentOrders, currentClients) = workOrder match {
      case Some(orderWork) =>
        val clientOrder = changeClientInfoWithOrder(order, clients)
        val clientWork = changeClientInfoWithOrder(orderWork, clients)
        val currentOrders = orders.filterNot(o => o.id == order.id || o.id == orderWork.id)
        // Опять таки верим в идеальные данный
        val currentClients = clients.filterNot(c => c.name == clientWork.get.name || c.name == clientOrder.get.name) ++
          clientWork ++ clientOrder
        (currentOrders, currentClients)
      case _ =>
        (orders ++ Some(order), clients)
    }

    if (ordersLine.nonEmpty) {
      val (newOrder, newOrderLine) = getOrderFromLine(ordersLine)
      makeOrder(newOrder.get, // все еще верим в идеальные данные
        currentClients,
        currentOrders,
        newOrderLine)

    } else {
      writeClients(currentClients)
    }
  }

  private def compareOrders(orderFirst: Order, orderSecond: Order) = {
    orderSecond.securitiesType == orderFirst.securitiesType &&
      orderSecond.count == orderFirst.count &&
      orderSecond.cost == orderFirst.cost &&
      // так как всего два вида операций
      orderSecond.operation != orderFirst.operation
  }

  private def writeClients(clients: Seq[Client]): Unit = {
    val clientsString = clients.sortBy(_.name).map(c => Seq(c.name, c.money, c.balanceA, c.balanceB, c.balanceC, c.balanceD).mkString("\t"))

    val file = new File("./src/main/scala/data/clients_results.txt")

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))

    clientsString.map(s => writer.write(s + "\n"))

    writer.close()

  }


  protected def getOrderFromLine(ordersLine: Seq[String]): (Option[Order], Seq[String]) = {
    val (currentOrderLine, tailOrders) = ordersLine.splitAt(1)
    val order = currentOrderLine.headOption.map { line =>
      line.split("\t") match {
        // Искренне верим, что формат данных правельный и не обрабытвываем исключения
        case Array(name: String,
        operation: String,
        securities: String,
        cost: String,
        count: String) =>
          Order(name,
            OperationType.apply(operation),
            SecuritiesType.apply(securities),
            cost.toInt,
            count.toInt,
            UUID.randomUUID)
      }
    }

    (order, tailOrders)
  }

  protected def changeClientInfoWithOrder(order: Order, currentClients: Seq[Client]) = {
    currentClients.find(_.name == order.name)
      .map { client =>
        val index = order.operation match {
          case OperationType.buy => 1
          case OperationType.sell => -1
        }

        val money = client.money - index * order.cost * order.count
        order.securitiesType match {
          case SecuritiesType.securitiesA =>
            client.copy(balanceA = calculateBalance(client.balanceA, index, order.count), money = money)
          case SecuritiesType.securitiesB =>
            client.copy(balanceB = calculateBalance(client.balanceB, index, order.count), money = money)
          case SecuritiesType.securitiesC =>
            client.copy(balanceC = calculateBalance(client.balanceC, index, order.count), money = money)
          case SecuritiesType.securitiesD =>
            client.copy(balanceD = calculateBalance(client.balanceD, index, order.count), money = money)
        }
      }
  }


  protected def calculateBalance(balance: Int, index: Int, count: Int): Int = {
    balance + index * count
  }

  private def readFile(fileName: String, enc: String) = {
    Source.fromFile(fileName, enc)
      .getLines
      .toSeq
  }
}
