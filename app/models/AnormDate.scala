package models

import org.joda.time._
import java.math.BigDecimal

case class AnormDate( id: Long, dateDebut: Option[DateMidnight], dateFin: Option[DateTime], dateModif: Option[DateTime] )


object AnormDate{
    import anorm._
    import anorm.SqlParser._
    import utils.AnormExtension._

    import play.api.Play.current
    import play.api.db._
    import play.api._

    val anormDateParser = {
        get[Long]("ANORM_DATE.id") ~
        get[Option[DateMidnight]]("ANORM_DATE.date_debut") ~
        get[Option[DateTime]]("ANORM_DATE.date_fin") ~
        get[Option[DateTime]]("ANORM_DATE.date_modif") map {
            case id ~ dateDebut ~ dateFin ~ dateModif => AnormDate( id, dateDebut, dateFin, dateModif )
        }
    }

    val genereratedKeyParser = {
        get[Long]("SCOPE_IDENTITY()") map {
            case id => id
        }
    }

    def create( dateDebut: Option[DateMidnight], dateFin: Option[DateTime], dateModif: Option[DateTime] ): Option[Long] = DB.withConnection { implicit c =>
        val query = SQL(
            """
                INSERT INTO ANORM_DATE ( date_debut, date_fin, date_modif ) VALUES ( {dateDebut}, {dateFin}, {dateModif})
            """
        )
        query.onParams( dateDebut, dateFin, dateModif ).executeInsert( genereratedKeyParser* ).headOption
    }

    def getAD( id: Long ): Option[AnormDate] = DB.withConnection { implicit c =>
        val query = SQL(
            """
                SELECT * FROM ANORM_DATE WHERE id = {id}
            """
        )
        query.onParams( id ).as(anormDateParser*).headOption
    }
}