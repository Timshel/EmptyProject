package parsers

import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.util.matching._
import java.math._
import org.joda.time._
import org.joda.time.format._
import play._

trait CsvParser[T] extends RegexParsers{

    override def skipWhitespace = false

    def separator: String

    def encoding: io.Codec = io.Codec("UTF-8")

    def EOF: Parser[String] = """\Z""".r

    def EOL: Parser[String] = (("\r"?) ~> "\n") | EOF

    def token: Parser[String] = ("""[^""" + separator + """\r\n]*""").r

    def line( tokens: Int ): Parser[List[String]] = token ~ repN(tokens-1, separator ~> token) <~ EOL ^^ { case token ~ list => token +: list }

    def nonConcordance = sys.error("Non concordance entre le nombre d'élement de la ligne et la fonction de mapping")

    def parser: Parser[Seq[T]]

    def parse( reader: Reader[Char] ): Either[String,Seq[T]] = {
        parser( reader ) match {
            case Success(sequence, in) if in.atEnd => Right(sequence)
            case Success(_, in) => Left("Oops, non parsé totalement -> " + in.pos.toString)
            case NoSuccess(message, _) => Left(message)
        }
    }

    def parse( data: String ): Either[String,Seq[T]] = {
        parse( new CharSequenceReader(data) )
    }

    def parsePath( path: String ): Either[String,Seq[T]] = {
        parse(io.Source.fromFile( path )(encoding).mkString)
    }

    def parseResource( path: String ): Either[String,Seq[T]] = {
        parse(io.Source.fromInputStream(Play.application().resourceAsStream(path))(encoding).mkString)
    }

    def optionBigDecimal( num: String ) : Option[BigDecimal] = {
        Option(num).filterNot( _.isEmpty ).map(new BigDecimal(_))
    }

    def optionString( str: String ) : Option[String] = {
        Option(str).filterNot( _.isEmpty ).map( identity )
    }

    def dateFormat: DateTimeFormatter;

    def parseDateTime( date: String ): DateTime = {
        dateFormat.parseDateTime(date)
    }

    def optionDateTime( date: String ): Option[DateTime] = {
        Option(date).filterNot( _.isEmpty ) match {
            case Some(s) => Some(parseDateTime(s))
            case _ => None
        }
    }
}

trait CsvEnumerateeParser[T] extends CsvParser[T] with EnumerateeParser[T]{

    def simpleParser: Parser[T]

    def simpleParse( reader: Reader[Char] ): Either[String,T] = {
        simpleParser( reader ) match {
            case Success(element, in) if in.atEnd => Right(element)
            case Success(_, in) => Left("Oops, non parsé totalement -> " + in.pos.toString)
            case NoSuccess(message, _) => Left(message)
        }
    }

    def simpleParse( data: String ): Either[String, T] = {
        simpleParse( new CharSequenceReader(data) )
    }

}


object CalculSiret{
    /**
    * Méthode permettant de calculer le chiffre de controle d'un SIRET.
    **/
    def siret13to14( siret: String ): String = {
        siret match {
            case s if s.matches("""\d{13}""") => {
                val sum = siret.zipWithIndex.foldLeft[Int](0){ ( sum, t ) =>
                    val cur = ( t._1.toInt - '0'.toInt ) match {
                        case x if t._2 % 2 == 0 => ( x * 2 ) % 10 + x / 5
                        case x => x
                    }
                    sum + cur
                }
                s + ( ( 10 - ( sum % 10 ) ) % 10 ).toString
            }
            case _ => siret
        }
    }
}