import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.{HttpRoutes, QueryParamDecoder}
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.{OptionalQueryParamDecoderMatcher, QueryParamDecoderMatcher}

import java.time.Year
import scala.util.Try

object Movie extends IOApp {

    type Actor = String
    case class Movie(id:String, title:String, year:Int, actors:List[String], director: String)
    case class Director(firstName:String, lastName: String) {
        override def toString: String = s"$firstName $lastName"
    }

    object DirectorQueryParamMatcher extends QueryParamDecoderMatcher[String]("director")
    //The QueryParamDecoderMatcher requires the name of the parameter and its type
    object YearQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Year]("year")

    //create an implicit queryParamDecoder that can be used by our YearQueryParamDecoderMatcher
    implicit val yearQueryParamDecoder: QueryParamDecoder[Year] = QueryParamDecoder[Int].map(x=> Year.of(x))


    //GET / movies ? director = Zack % 20 Snyder & year = 2021
    //GET /movies/aa4f0f9c-c703-4f21-8c05-6a0c8f2052f0/actors
    def movieRoutes[F[_]: Monad]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._
        HttpRoutes.of[F] {
            case GET -> Root / "Movies" :? DirectorQueryParamMatcher(director) +& YearQueryParamMatcher(year) => ???
            case GET -> Root/ "Movies"/ UUIDVar(movieId) / "actors" => ???
            //UUIDVar is a route extractor used to match and convert path segments into UUID objects.
        }
        // we parameterise the route definition with an effect F, as we probably have to retrieve some information from
        // some external source
        // Each case statement represents a route,
    }

    //defining a custom Director route extractor

    object DirectorVar {
        def unapply(str: String): Option[Director] = {
            if(str.nonEmpty && str.matches(".*.*")) {
                Try {
                    val director = str.split(" ")
                    Director(director(0), director(1))
                }.toOption
            } else None
        }
    }
    def directorRoutes[F[_]: Monad]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._
        HttpRoutes.of[F] {
            case GET -> Root / "directors"/ DirectorVar(director) => ???
        }
    }

    override def run(args: List[String]): IO[ExitCode] = ???
}

/*
* Very often, producing a Response from a Request means interacting with databases, external
* services, and so on, which may have some side effects. In order to maintain the referential
*  transparency of our functions. Hence, the library surrounds the Response type into an
* effect F[_]. So, we change the previous route definition in Request => F[Response].
*
*
*Nevertheless, not all the Request will find a route to a Response. So, we need to take into consideration this fact,
* defining a route as a function of type Request => F[Option[Response]]. Using a monad transformer, we can translate
* this type in Request => OptionT[F, Response].*/