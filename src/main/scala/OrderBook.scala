import cats.Monad
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import org.http4s.{ContentCoding, EntityDecoder, HttpRoutes, ResponseCookie}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.syntax.kleisli.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import org.http4s.circe.jsonOf
import org.http4s.headers.`Content-Encoding`
import cats.implicits.*


import java.util.UUID
import scala.collection.mutable
import scala.concurrent.ExecutionContext.global

object OrderBook extends IOApp{

    enum Type:
        case Buy, Sell

    case class Order(OrderId:UUID, userId:String, instrument: String, orderType: Type, price: Int, quantity: Int)
    //Order consist of OrderId, userId, instrument, type(buy sell), price, quantity

    //we can use a map to store userId mapped to the list of existing order the user has made
    val orderMap: mutable.Map[String, List[Order]] = mutable.Map();

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
                    _ = orderMap.put(order.userId, existingOrders)
                    res <- Ok.headers(`Content-Encoding`(ContentCoding.gzip))
                } yield res
//            case GET -> Root / "orders" =>

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
