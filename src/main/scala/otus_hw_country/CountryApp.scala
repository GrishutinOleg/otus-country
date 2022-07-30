package otus_hw_country

import io.circe
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import io.circe.parser
import io.circe.generic.semiauto._
import io.circe._

import scala.language.implicitConversions
import io.circe.{Decoder, Json}


case class Country1(
                     name: Name,
                     capital: List[String],
                     area: Double,
                     region: String)

case class Name(common: String)

case class CountryDto(
                       name: String,
                       capital: String,
                       area: Double
                     )

object CountryDto {
  def createDto(country: Country1): CountryDto =
    CountryDto(
      name = country.name.common,
      capital = country.capital.head,
      area = country.area
    )
}


object CountryApp extends App{
  def using[A, B <: {def close(): Unit}] (closeable: B) (f: B => A): A =
    try { f(closeable) } finally { closeable.close() }

  def decodeListTolerantly[A: Decoder]: Decoder[List[A]] =
    Decoder.decodeList(Decoder[A].either(Decoder[Json])).map(
      _.flatMap(_.left.toOption)
    )
  val myTolerantFooDecoder = decodeListTolerantly[Country1]

  val result = using(scala.io.Source.fromFile("src/main/resourses/countries.json")) { src =>
    val decoded = decode(src.mkString)(myTolerantFooDecoder)

    decoded match {
      case Right(value) =>
        value
          .filter(_.region == "Africa")
          .sortBy(_.area)(Ordering[Double].reverse)
          .slice(0, 10)
          .map(CountryDto.createDto)
          .asJson
          .noSpaces
      case Left(value) => throw new RuntimeException(s"Parsing problems: $value")
    }
      }

  val parseResult = circe.parser.parse(result)

  parseResult match {
    case Right(value) =>
      value
        .asJson
        .noSpaces
    case Left(value) => throw new RuntimeException(s"Parsing problems: $value")

  }
  val countryresult = parseResult.getOrElse()

  println(countryresult)
  println(countryresult.getClass)

}
