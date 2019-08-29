import java.util.UUID

import models.{Client, OperationType, Order, SecuritiesType}
import org.scalatest.{Matchers, WordSpec}
class OrderWorkerTest extends WordSpec
  with Matchers {

  // Естественно тестов должно быть больше, но 00:00 на часах внесло свои коррективы
  implicit val orderWorker = new OrderWorker {
    def calculateBalanceTest(balance: Int, index: Int, count: Int): Int =
      super.calculateBalance(balance: Int, index: Int, count: Int)

    def changeClientInfoTest(order: Order, currentClients: Seq[Client]): Option[Client] =
      super.changeClientInfoWithOrder(order, currentClients)

    def getOrderFromLineTest(ordersLine: Seq[String]): (Option[Order], Seq[String]) =
      super.getOrderFromLine(ordersLine)
  }

  "OrderWorker" should {
    "calculate balance for sell (index -1) " in {
      orderWorker.calculateBalanceTest(100, -1, 2) shouldBe 98
    }

    "calculate balance for buy (index 1) " in {
      orderWorker.calculateBalanceTest(100, 1, 2) shouldBe 102
    }


    "change client info with order A to buy" in {
      val client1 = Client("C1", 1000, 130, 240, 760, 320)
      val client2 = Client("C2", 2000, 230, 200, 100, 200)
      val clients = Seq(client1, client2)

      val uuid = UUID.randomUUID()
      val order = Order("C1", OperationType.buy, SecuritiesType.apply("A"), 10, 5, uuid)

      val data = Some(Client("C1", 950, 135, 240, 760, 320))
      orderWorker.changeClientInfoTest(order, clients) shouldBe data
    }

    "change client info with order C to sell" in {
      val client1 = Client("C1", 1000, 130, 240, 760, 320)
      val client2 = Client("C2", 2000, 230, 200, 100, 200)
      val clients = Seq(client1, client2)

      val uuid = UUID.randomUUID()
      val order = Order("C2", OperationType.sell, SecuritiesType.apply("C"), 10, 5, uuid)

      val data = Some(Client("C2", 2050, 230, 200, 95, 200))
      orderWorker.changeClientInfoTest(order, clients) shouldBe data
    }

    "change client info with order D to buy" in {
      val client1 = Client("C1", 1000, 130, 240, 760, 320)
      val client2 = Client("C2", 2000, 230, 200, 100, 200)
      val clients = Seq(client1, client2)

      val uuid = UUID.randomUUID()
      val order = Order("C1", OperationType.buy, SecuritiesType.apply("D"), 10, 5, uuid)

      val data = Some(Client("C1", 950, 130, 240, 760, 325))
      orderWorker.changeClientInfoTest(order, clients) shouldBe data
    }

    "change client info with order B to sell" in {
      val client1 = Client("C1", 1000, 130, 240, 760, 320)
      val client2 = Client("C2", 2000, 230, 200, 100, 200)
      val clients = Seq(client1, client2)

      val uuid = UUID.randomUUID()
      val order = Order("C2", OperationType.sell, SecuritiesType.apply("B"), 10, 5, uuid)

      val data = Some(Client("C2", 2050, 230, 195, 100, 200))
      orderWorker.changeClientInfoTest(order, clients) shouldBe data
    }

    "get order from line" in {
      val lines = Seq("C4\ts\tC\t14\t2", "C8\tb\tB\t7\t5")
      val uuid = UUID.randomUUID()
      val data = (Some(Order("C4", OperationType.sell, SecuritiesType.apply("C"), 14, 2, uuid)), Seq("C8\tb\tB\t7\t5"))
      val (order, seqOrdersLine) = orderWorker.getOrderFromLineTest(lines)
      (order.map(_.copy(id = uuid)), seqOrdersLine) shouldBe data
    }
  }
}

