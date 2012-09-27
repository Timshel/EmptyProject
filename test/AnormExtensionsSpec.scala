
import org.specs2.mutable._

import play.api._
import play.api.test._
import play.api.test.Helpers._
import models._

import org.joda.time._


class AnormExtensionsSpec extends Specification{

    Play.start( FakeApplication() )

    "Mapping of a DateMidnight to a Date field" should {
        val date = new DateMidnight()
        val id = models.AnormDate.create( Option(date), None, None )

        "A row must successfully be inserted" in { id must be }

        val anormDate = models.AnormDate.getAD( id.get )

        "A row must successfully be readed with a value" in {
            anormDate must be
            anormDate.get.dateDebut must be
        }

        "The value should still be the same" in { anormDate.get.dateDebut.get mustEqual date }
    }

    "Mapping of a DateTime to a Date field" should {
        val date = new DateTime()
        val id = models.AnormDate.create( None, Option(date), None )

        "A row must successfully be inserted" in { id must be }

        val anormDate = models.AnormDate.getAD( id.get )

        "A row must successfully be readed with a value" in {
            anormDate must be
            anormDate.get.dateFin must be
        }

        "The value should not still be the same" in { anormDate.get.dateFin.get mustNotEqual date }
    }

    "Mapping of a DateTime to a DateTime field" should {
        val date = new DateTime()
        val id = models.AnormDate.create( None, None, Option(date) )

        "A row must successfully be inserted" in { id must be }

        val anormDate = models.AnormDate.getAD( id.get )

        "A row must successfully be readed with a value" in {
            anormDate must be
            anormDate.get.dateModif must be
        }

        "The value should still be the same" in { anormDate.get.dateModif.get mustEqual date }
    }

}