package parsers

import play._
import play.api.libs.iteratee._
import play.api.libs.iteratee.Enumeratee._
import play.api.libs.concurrent._

import java.io.File

trait EnumerateeParser[T] {

    val needle: String

    def simpleParse( data: String ): Either[String, T]

    def grouped[From] = new {
        def apply[To](folder: Iteratee[From, To]): Enumeratee[From, To] = new CheckDone[From, To] {
            def step[A](f: Iteratee[From, To])(k: K[To, A]): K[From, Iteratee[To, A]] = {
                case in @ (Input.El(_) | Input.Empty) =>
                    Iteratee.flatten(f.feed(in)).pureFlatFold(
                        (a, _) => new CheckDone[From, To] { def continue[A](k: K[To, A]) = Cont(step(folder)(k)) } &> k(Input.El(a)),
                        kF => Cont(step(Cont(kF))(k)),
                        (msg, e) => Error(msg, in))

                case Input.EOF => Iteratee.flatten(f.run.map( (c:To) => Done(k(Input.El(c)), Input.EOF) ))
            }

            def continue[A](k: K[To, A]) = Cont(step(folder)(k))
        }
    }

    val splitChunk: Enumeratee[Array[Byte],T] = (
        Parsing.search(needle.getBytes) ><>
        grouped(
            Enumeratee.breakE[Parsing.MatchInfo[Array[Byte]]](_.isMatch) ><>
            Enumeratee.map{ mi => new String(mi.content) }  &>>
            Iteratee.consume()
        ) ><>
        Enumeratee.filter{ _.length > 0} ><>
        Enumeratee.map{ data => simpleParse(needle + data).right.get }
    )

    val consume2 = Iteratee.consume[String]()

    private def parseA[E]( input: Enumerator[Array[Byte]], consume: Iteratee[T,E], f: E => Boolean ): Boolean = {
        val eventuallyResult: Promise[E] = { Iteratee.flatten( input |>> splitChunk &>> consume ).run }

        f( eventuallyResult.value.get )
    }

    def parsePathA[E]( path: String, consume: Iteratee[T,E], f: E => Boolean ): Boolean = {
        parseA( Enumerator.fromFile( new File( path ) ), consume, f )
    }

    def parseResourceA[E]( path: String, consume: Iteratee[T,E], f: E => Boolean ): Boolean = {
        parseA( Enumerator.fromStream( Play.application().resourceAsStream(path) ), consume, f )
    }

}