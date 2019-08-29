object Main extends App{
    val orderWorker = new OrderWorker
    // пути лучще вынести в настройки
    val inputClients = orderWorker.readClients("./src/main/scala/data/clients.txt", "ASCII")
    orderWorker.processOrder("./src/main/scala/data/orders.txt", "ASCII", inputClients)
}
