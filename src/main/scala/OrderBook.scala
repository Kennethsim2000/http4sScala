import OrderBook.Type.{Buy, Sell}
import cats.Monad
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import org.http4s.{ContentCoding, EntityDecoder, HttpApp, HttpRoutes, ResponseCookie}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.syntax.kleisli.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import cats.implicits.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.circe.*
import io.circe.{Decoder, Encoder}

import scala.collection.mutable.PriorityQueue
import java.util.UUID
import scala.collection.mutable
import scala.concurrent.ExecutionContext.global

object OrderBook extends IOApp{

    enum Type:
        case Buy, Sell

    // we need to define custom encoders and decoders
    given Encoder[Type] = Encoder.encodeString.contramap(_.toString)
    given Decoder[Type] = Decoder.decodeString.emap {
        case "Buy" => Right(Type.Buy)
        case "Sell" => Right(Type.Sell)
        case other => Left(s"Unknown orderType: $other")
    }

    case class Order(orderId:UUID, userId:String, instrument: String, orderType: Type, price: Int, quantity: Int)
    //Order consist of OrderId, userId, instrument, type(buy sell), price, quantity

    case class OrderReq(userId:String, instrument: String, orderType: Type, price: Int, quantity: Int)

    case class Transaction(transactionId: UUID, userId: String, instrument: String, orderType: Type, price: Int, quantity: Int)

    //we can use a map to store userId mapped to the list of existing order that the user currently has(not fulfilled)
    val orderMap: mutable.Map[String, List[Order]] = mutable.Map();

    //Map that maps userId to a list of orders that this user has transacted
    val transactionMap: mutable.Map[String, List[Transaction]] = mutable.Map();

    //used for deleted orders, so we do not match them in matchOrder
    val deletedOrders: mutable.Set[UUID] = mutable.Set();

    val buyMap: mutable.Map[String, mutable.PriorityQueue[Order]] = mutable.Map()
    // map each instrument to each corresponding priorityqueue containing orders
    val sellMap: mutable.Map[String, mutable.PriorityQueue[Order]] = mutable.Map()

    //orderings
    implicit val buyOrdering: Ordering[Order] = Ordering.by[Order, Int](_.price).reverse
    // buy ordering must be sorted in descending order, with the highest buy first
    implicit val sellOrdering: Ordering[Order] = Ordering.by[Order, Int](_.price)
    //we have to add in the [Order, Int] if not we will get price is not a member of any

    def matchOrder(order: OrderReq): Option[Order] = {

        if(order.orderType == Type.Buy) {
            val sellOrders = sellMap.getOrElseUpdate(order.instrument, mutable.PriorityQueue.empty[Order](using sellOrdering))
            val sellList = sellOrders.dequeueAll.toList // we dequeue all to list
            val (remainingQty, updatedSellList) = matchLoop(order, sellList)
            val sellPriorityQueue = mutable.PriorityQueue(updatedSellList*)(using sellOrdering)
            sellMap.put(order.instrument, sellPriorityQueue)

            // If the order is not fulfilled, add a buy order to the buy orderbook
            if (remainingQty > 0) {
                val buyOrder = Order(UUID.randomUUID(), order.userId, order.instrument, order.orderType,
                    order.price, remainingQty)
                val buyPriorityQueue = buyMap.getOrElseUpdate(order.instrument, mutable.PriorityQueue.empty[Order](using buyOrdering))
                buyPriorityQueue.enqueue(buyOrder)
                buyMap.put(order.instrument, buyPriorityQueue)
                Some(buyOrder)
            } else {
                None
            }


        } else { // order type is sell, obtain the buy orderbook
            val buyOrders = buyMap.getOrElseUpdate(order.instrument, mutable.PriorityQueue.empty[Order](using buyOrdering))
            val buyList = buyOrders.dequeueAll.toList // we dequeue all to list
            val (remainingQty, updatedBuyList) = matchLoop(order, buyList)
            val buyPriorityQueue = mutable.PriorityQueue(updatedBuyList *)(using sellOrdering)
            buyMap.put(order.instrument, buyPriorityQueue)

            if (remainingQty > 0) {
                val sellOrder = Order(UUID.randomUUID(), order.userId, order.instrument, order.orderType,
                    order.price, remainingQty)
                val sellPriorityQueue = sellMap.getOrElseUpdate(order.instrument, mutable.PriorityQueue.empty[Order](using buyOrdering))
                sellPriorityQueue.enqueue(sellOrder)
                sellMap.put(order.instrument, sellPriorityQueue)
                Some(sellOrder)
            } else {
                None
            }
        }

    }

    def matchLoop(order: OrderReq, orderList: List[Order]): (Int, List[Order]) = {
        // returns unfilled quantity, as well as updated order book
        val remainingOrders = orderList.foldLeft((order.quantity, List.empty[Order])) {
            case ((0, currOrderBook ), currentOrder) => (0, currOrderBook :+ currentOrder) // if quantity left is 0, meaning order is filled
            case ((quantity, currOrderBook), currentOrder) =>
                val canMatch = {
                    if(order.orderType == Buy) order.price >= currentOrder.price
                    else order.price <= currentOrder.price
                }
                if (canMatch) {
                    val transactedQuantity = math.min(order.quantity, currentOrder.quantity)
                    println(s"Matched order $order.orderId for $transactedQuantity units")

                    // Transactions for the incoming order
                    val incomingOrderTransactions = transactionMap.getOrElse(order.userId, List())
                    val incomingTransaction = Transaction(
                        UUID.randomUUID(), order.userId, order.instrument, order.orderType,
                        order.price, transactedQuantity
                    )
                    transactionMap.put(order.userId, incomingOrderTransactions :+ incomingTransaction)

                    // Transactions for the existing order in the book
                    val existingOrderTransactions = transactionMap.getOrElse(currentOrder.userId, List())
                    val existingTransaction = Transaction(
                        UUID.randomUUID(), currentOrder.userId, currentOrder.instrument,
                        currentOrder.orderType, order.price, transactedQuantity
                    )
                    transactionMap.put(currentOrder.userId, existingOrderTransactions :+ existingTransaction)

                    val remainingQuantity = quantity - transactedQuantity
                    val currOrderQuantity = currentOrder.quantity - transactedQuantity
                    if(currOrderQuantity == 0) {
                        (remainingQuantity, currOrderBook) // our current order has filled up, do not add back to orderBook
                    } else {
                        (remainingQuantity, currOrderBook :+ currentOrder.copy(quantity = currOrderQuantity))
                        // add the order with remaining quantity back to orderbook
                    }
                } else { // cannot match already
                    (quantity, currOrderBook :+ currentOrder) // add our current order to our orderbook because we cannot match
                }
        }
        return remainingOrders

    }

    object UserQueryParamMatcher extends QueryParamDecoderMatcher[String]("user")

    // POST Order to add a Order to the orderbook: /api/orders
    // GET Order to view all unfilled orders of a particular user: /api/orders?user=kenneth
    // DELETE orderId to delete a particular order /api/orders/312312412
    def orderRoutes[F[_] : Concurrent]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._
        implicit val orderReqDecoder: EntityDecoder[F, OrderReq] = jsonOf[F, OrderReq]

        HttpRoutes.of[F] {
            case req@POST -> Root/ "orders" =>
                for {
                    order <- req.as[OrderReq]
                    remainingOrder = matchOrder(order) // call match order to insert
                    existingOrders = orderMap.getOrElse(order.userId, List())
                    newOrders = remainingOrder.fold(existingOrders)(existingOrders :+ _)
                    _ = orderMap.put(order.userId, newOrders)
                    res <- Ok(order.asJson)
                } yield res
            case GET -> Root / "orders" :? UserQueryParamMatcher(user) =>
                    val orders = orderMap.getOrElse(user, Seq())
                    Ok(orders.asJson) //requires org.http4s.circe._
                    //you’re returning a Circe Json object (orders.asJson), but http4s doesn't automatically know how to
            // turn that into an HTTP response body — unless you provide an implicit EntityEncoder for that type.

            case DELETE -> Root / "orders" / UUIDVar(orderId) => ???
            //TODO: Implement delete orderId(using the deleted set)

        }

    }

    // GET all transacted orders of a user /api/users?user=kenneth
    // GET the profits/loss of a user /api/users/profits?user=kenneth
    def userRoutes[F[_] : Concurrent]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._

        HttpRoutes.of[F] {
            case GET -> Root / "users" :? UserQueryParamMatcher(user) =>
                val transactions = transactionMap.getOrElse(user, List())
                Ok(transactions.asJson)
            //TODO: Obtain the profits of a user
            case GET -> Root / "users" / "profits" :? UserQueryParamMatcher(user) => ???

        }

    }

    def allRoutes[F[_] : Concurrent]: HttpRoutes[F] = {
        import cats.syntax.semigroupk._
        orderRoutes[F] <+> userRoutes[F]
    }

    def allRoutesComplete[F[_] : Concurrent]: HttpApp[F] = {
        allRoutes.orNotFound
    }
    
    override def run(args: List[String]): IO[ExitCode] = {
        import cats.syntax.semigroupk._
        val apis = OrderBook.allRoutesComplete[IO]
        BlazeServerBuilder[IO](global)
            .bindHttp(8080, "localhost")
            .withHttpApp(apis)
            .resource
            .use(_=>IO.never)
            .as(ExitCode.Success)
    }
}

