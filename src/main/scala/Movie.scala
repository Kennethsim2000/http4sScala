import cats.*
import cats.effect.*
import cats.implicits.*
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.{OptionalValidatingQueryParamDecoderMatcher, QueryParamDecoderMatcher}
import org.http4s.syntax.kleisli.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import org.http4s.circe.*
import org.http4s.headers.`Content-Encoding`
import org.typelevel.ci.CIString

import java.time.Year
import java.util.UUID
import scala.collection.mutable
import scala.util.Try

object Movie extends IOApp {

    type Actor = String
    case class Movie(id:String, title:String, year:Int, actors:List[String], director: String)
    case class Director(firstName:String, lastName: String) {
        override def toString: String = s"$firstName $lastName"
    }

    val movie1: Movie = Movie("6bcbca1e-efd3-411d-9f7c-14b872444fce", "How to train your dragon", 2019,
        List("kenneth", "Tom cruise", "TOP"), "Kenneth sim")
    val movieMap: Map[String, Movie] = Map(movie1.id -> movie1);

    def findById(movieId: UUID): Option[Movie] = {
        movieMap.get(movieId.toString);
    }
    def findByDirector(director: String):List[Movie] = {
        movieMap.values.filter(_.director == director).toList
    }


    object DirectorQueryParamMatcher extends QueryParamDecoderMatcher[String]("director")
    //The QueryParamDecoderMatcher requires the name of the parameter and its type
    object YearQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[Year]("year")
    //this returns a Option[ValidatedNel[ParseFailure, A]]
    //In Cats, Validated is a data type that represents a value that is either:
    //Valid[A] – success case, holds a value of type A
    //Invalid[E] – failure case, holds an error (often a collection like NonEmptyList[E])
    //
    //This is similar to Either, but unlike Either, Validated is often used for accumulating errors, especially in form
    // or query param validation.

    //create an implicit queryParamDecoder that can be used by our YearQueryParamDecoderMatcher
    implicit val yearQueryParamDecoder: QueryParamDecoder[Year] = QueryParamDecoder[Int]
        .emap { y=>
            Try(Year.of(y)) //Try type represents a computation that may either result in an exception, or return a successfully computed value
                .toEither
                .leftMap { tr=>
                    ParseFailure(tr.getMessage, tr.getMessage)
                }
            }
    //emap returns an Either, where the leftMap is a ParseFailure
    //ParseFailure is a type of the http4s library indicating an error parsing an HTTP Message


    //GET / movies ? director = Zack % 20 Snyder & year = 2021
    //GET /movies/aa4f0f9c-c703-4f21-8c05-6a0c8f2052f0/actors
    def movieRoutes[F[_]: Monad]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._
        HttpRoutes.of[F] {
            case GET -> Root / "Movies" :? DirectorQueryParamMatcher(director) +& YearQueryParamMatcher(maybeYear) =>
                maybeYear match {
                    case Some(y) => y.fold( // cats validated.fold
                        _ => BadRequest("The given year is not valid"), //invalid
                        {
                            validYear => // valid
                                val movieByDirector = findByDirector(director)
                                val movieByDirectorAndYear = movieByDirector.filter(_.year == validYear.getValue)
                                Ok(movieByDirectorAndYear.asJson)

                        }
                    )
                    case None => Ok(findByDirector(director).asJson)
                } //The method .fold on Validated[E, A] lets you handle both cases (Invalid and Valid) in a functional way.
            case GET -> Root/ "Movies"/ UUIDVar(movieId) / "actors" =>
                findById(movieId).map(_.actors) match {
                    case Some(actors) => Ok(actors.asJson)
                    case None => NotFound(s"No movie with $movieId is found")
                }
            //UUIDVar is a route extractor used to match and convert path segments into UUID objects.
        }
        // we parameterise the route definition with an effect F, as we probably have to retrieve some information from
        // some external source
        // Each case statement represents a route,
    }

    //defining a custom Director route extractor, used to extract a Director from the path
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

    // mutable map mapping actor to director
    val directorMap:mutable.Map[Actor, Director] = mutable.Map("Zack Snyder" -> Director("Zack", "Snyder"));


    //Concurrent[F] is a type class from Cats Effect that describes effect types that support concurrency and
    // cancelation, like IO, Task, ZIO
    //jsonOf[F, A] is the http4s + Circe integration that creates an EntityDecoder[F, A]. It’s used to parse JSON from
    // an HTTP request body into a Scala type
    def directorRoutes[F[_]: Concurrent]: HttpRoutes[F] = {
        val dsl = Http4sDsl[F]
        import dsl._
        implicit val directorDecoder: EntityDecoder[F, Director] = jsonOf[F, Director]

        // requires a Concurrent constraint instead of monad
        //Because reading a request body is asynchronous and cancelable, it requires Concurrent[F], not just Monad[F]
        HttpRoutes.of[F] {
            case GET -> Root / "directors"/ DirectorVar(director) =>
                directorMap.get(director.toString) match { // calling asJson requires an implict Encoder[A]
                    case Some(foundDirector) => Ok(foundDirector.asJson, Header.Raw(CIString("my-custom-Header"), "value"))
                    case None => NotFound(s"No director called $director found")
                }
            case req@POST -> Root/ "directors" =>
                for {
                    director <- req.as[Director] // need to import cats.implicits._
                    _ = directorMap.put(director.toString, director)
                    res <- Ok.headers(`Content-Encoding`(ContentCoding.gzip))
                        .map(_.addCookie(ResponseCookie("My-Cookie", "value")))
                } yield res
        }
    }

    //composing routes
    def allRoutes[F[_]: Concurrent] :HttpRoutes[F] = {
        import cats.syntax.semigroupk._
        movieRoutes[F] <+> directorRoutes[F]
    }
    //handle if some routes do not match with any of the available routes
    def allRoutesComplete[F[_]: Concurrent]: HttpApp[F] = {
        allRoutes.orNotFound
    }
    /* As we can see, the method returns an instance of the type HttpApp[F], which is a type alias for
    Kleisli[F, Request[F], Response[F]]. Hence, for what we said at the beginning of this article, this Kleisli is
    nothing more than a wrapper around the function Request[G] => F[Response[G]]. So, the difference with the
    HttpRoutes[F] type is that we removed the OptionT on the response. */



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

/*
* Circe Encoder[A] – knows how to convert a Scala object into JSON

Http4s EntityEncoder[F, A] – knows how to send an A in an HTTP response (e.g. with proper content-type, charset, etc.)
*/

/** IMPORTS AND THEIR MEANINGS
    * import cats.implicits.* // required for req.as[Director]
    * import io.circe.syntax.* // bring into scope asJson
    * import io.circe.generic.auto.* //Automatically generates Encoder and Decoder instances for your case classes.
*/