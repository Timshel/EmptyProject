package utils

import org.joda.time.DateTime
import play.api.mvc._

object Binder{

    implicit def dateTimePathBindable(implicit longBinder: PathBindable[Long]) = new PathBindable[DateTime] {
        def bind(key: String, value: String): Either[String, DateTime] = {
            longBinder.bind(key, value).right.map{
                new DateTime(_)
            }
        }

        def unbind(key: String, date: DateTime): String = {
            longBinder.unbind(key, date.getMillis)
        }
    }

}

