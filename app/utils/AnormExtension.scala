package utils

object AnormExtension {
    import anorm._

    implicit def rowToDateTime: Column[org.joda.time.DateTime] = Column.nonNull { (value, meta) =>
        import org.joda.time.DateTime
        val MetaDataItem(qualified, nullable, clazz) = meta
        value match {
            case d: java.sql.Date => Right( new DateTime( d.getTime ) )
            case t: java.sql.Timestamp => Right( new DateTime( t.getTime ) )
            case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to DateTime for column " + qualified) )
        }
    }

    implicit def rowToDateMidnight: Column[org.joda.time.DateMidnight] = Column.nonNull { (value, meta) =>
        val MetaDataItem(qualified, nullable, clazz) = meta
        value match {
            case d: java.sql.Date => Right( new org.joda.time.DateMidnight( d.getTime ) )
            case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to DateMidnight for column " + qualified) )
        }
    }

    implicit def readableInstantToStatement[A <: org.joda.time.ReadableInstant] = new ToStatement[A] {
        def set(s: java.sql.PreparedStatement, index: Int, aValue: A): Unit = {
            s.setTimestamp(index, new java.sql.Timestamp(aValue.getMillis()) )
        }
    }

}