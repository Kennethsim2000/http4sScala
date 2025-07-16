import OrderBook.Type.{Buy, Sell}
import cats.Monad
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import org.http4s.{ContentCoding, EntityDecoder, HttpRoutes, ResponseCookie}
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

    //be able to compute a user's profits as well.

    case class Order(OrderId:UUID, userId:String, instrument: String, orderType: Type, price: Int, quantity: Int)
    //Order consist of OrderId, userId, instrument, type(buy sell), price, quantity

    //we can use a map to store userId mapped to the list of existing order the user has made
    val orderMap: mutable.Map[String, List[Order]] = mutable.Map();

    val buyMap: mutable.Map[String, mutable.PriorityQueue[Order]] = mutable.Map() // map each instrument to each corresponding priorityqueue containing orders
    val sellMap: mutable.Map[String, mutable.PriorityQueue[Order]] = mutable.Map()

    //orderings
    implicit val buyOrdering: Ordering[Order] = Ordering.by[Order, Int](_.price).reverse // buy ordering must be sorted in descending order, with the highest buy first
    implicit val sellOrdering: Ordering[Order] = Ordering.by[Order, Int](_.price)
    //we have to add in the [Order, Int] if not we will get price is not a member of any

    def matchOrder(order: Order) = {

        if(order.orderType == Type.Buy) {
            val sellOrders = sellMap.getOrElseUpdate(order.instrument, PriorityQueue.empty[Order](sellOrdering))



        } else {

        }

    }

    def matchLoop(order: Order, orderList: List[Order]): (Int, List[Order]) = {
        // returns unfilled quantity, as well as updated order book
        val remainingOrders = orderList.foldLeft((order.quantity, List.empty[Order])) {
            case ((0, currOrderBook ), currentOrder) => (0, currOrderBook :+ currentOrder)
            case ((quantity, currOrderBook), currentOrder) =>
                val canMatch = {
                    if(order.orderType == Buy) order.price >= currentOrder.price
                    else order.price <= currentOrder.price
                }
                if (canMatch) {
                    val transactedQuantity = math.min(order.quantity, currentOrder.quantity)
                    println(s"Matched order $order.orderId for $transactedQuantity units")
                    val remainingQuantity = quantity - transactedQuantity
                    val currOrderQuantity = currentOrder.quantity - transactedQuantity
                    if(currOrderQuantity == 0) {
                        (remainingQuantity, currOrderBook) // our current order has filled up
                    } else {
                        (remainingQuantity, currOrderBook :+ currentOrder.copy(quantity = currOrderQuantity))
                        // our order
                    }
                } else { // cannot match already
                    (quantity, currOrderBook :+ currentOrder) // add our current order to our orderbook because we cannot match
                }

            // if it is a buy order, then it must be bigger than what the seller wants to sell at
        }
        return remainingOrders

    }

    object UserQueryParamMatcher extends QueryParamDecoderMatcher[String]("user")

    // POST Order to add a Order to the orderbook
    // GET Order to view all orders of a particular user
    // DELETE orderId to delete a particular order
    def orderRoutes[F[_] : Concurrent]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._
        implicit val orderDecoder: EntityDecoder[F, Order] = jsonOf[F, Order]

        HttpRoutes.of[F] {
            case req@POST -> Root/ "orders" =>
                for {
                    order <- req.as[Order]
                    existingOrders = orderMap.getOrElse(order.userId, List())
                    newOrders = existingOrders :+ order // scala list are immutable
                    _ = orderMap.put(order.userId, newOrders)
                    res <- Ok(order.asJson)
                } yield res
            case GET -> Root / "orders" :? UserQueryParamMatcher(user) =>
                    val orders = orderMap.getOrElse(user, Seq())
                    Ok(orders.asJson) //requires org.http4s.circe._
                    //you’re returning a Circe Json object (orders.asJson), but http4s doesn't automatically know how to
            // turn that into an HTTP response body — unless you provide an implicit EntityEncoder for that type.
        }

    }

    override def run(args: List[String]): IO[ExitCode] = {
        val apis = Router(
            "/api" -> OrderBook.orderRoutes[IO],
        ).orNotFound
        BlazeServerBuilder[IO](global)
            .bindHttp(8080, "localhost")
            .withHttpApp(apis)
            .resource
            .use(_=>IO.never)
            .as(ExitCode.Success)
    }
}
