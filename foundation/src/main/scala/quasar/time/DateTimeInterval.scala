/*
 * Copyright 2014–2018 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package quasar.time

import slamdata.Predef._

import java.time.{
  Duration,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
  Period
}

import scalaz.\/
import scalaz.Scalaz._

// TODO is this true?
// note that `parse <-> toString` is not an isomorphism, because the first minus sign
// is absorbed into the rest of the units in the interval,
// and nanos in excess of a second are added into seconds
//
// Let it be known: adding months does not behave as a monoid action on +.
// LocalDate.of(1, 1, 31).plusMonths(2) == LocalDate.of(1, 3, 31)
// LocalDate.of(1, 1, 31).plusMonths(1).plusMonths(1) == LocalDate.of(1, 3, 28)

final case class DateTimeInterval(period: Period, duration: Duration) {

  override def toString: String =
    if (duration.isZero) {
      period.toString
    } else if (period.isZero) {
      duration.toString
    } else {
      // drop the leading 'P' from the duration
      (period.toString: String) + (duration.toString.drop(1): String)
    }

  def plus(other: DateTimeInterval): DateTimeInterval =
    DateTimeInterval(period.plus(other.period), duration.plus(other.duration))

  def minus(other: DateTimeInterval): DateTimeInterval =
    DateTimeInterval(period.minus(other.period), duration.minus(other.duration))

  def multiply(factor: Int): DateTimeInterval =
    DateTimeInterval(period.multipliedBy(factor), duration.multipliedBy(factor.toLong))

  def subtractFromLocalDateTime(dt: LocalDateTime): LocalDateTime =
    dt.minus(period).minus(duration)

  def subtractFromLocalDate(dt: LocalDate): LocalDate =
    dt.minus(period)

  def subtractFromLocalTime(dt: LocalTime): LocalTime =
    dt.minus(duration)

  def subtractFromOffsetDateTime(odt: OffsetDateTime): OffsetDateTime =
    OffsetDateTime.of(subtractFromLocalDateTime(odt.toLocalDateTime), odt.getOffset)

  def subtractFromOffsetDate(odt: OffsetDate): OffsetDate =
    OffsetDate(subtractFromLocalDate(odt.date), odt.offset)

  def subtractFromOffsetTime(odt: OffsetTime): OffsetTime =
    OffsetTime.of(subtractFromLocalTime(odt.toLocalTime), odt.getOffset)

  def addToLocalDateTime(dt: LocalDateTime): LocalDateTime =
    dt.plus(period).plus(duration)

  def addToLocalDate(dt: LocalDate): LocalDate =
    dt.plus(period)

  def addToLocalTime(dt: LocalTime): LocalTime =
    dt.plus(duration)

  def addToOffsetDateTime(odt: OffsetDateTime): OffsetDateTime =
    OffsetDateTime.of(addToLocalDateTime(odt.toLocalDateTime), odt.getOffset)

  def addToOffsetDate(odt: OffsetDate): OffsetDate =
    OffsetDate(addToLocalDate(odt.date), odt.offset)

  def addToOffsetTime(odt: OffsetTime): OffsetTime =
    OffsetTime.of(addToLocalTime(odt.toLocalTime), odt.getOffset)

  def isDateLike: Boolean = duration.isZero
  def isTimeLike: Boolean = period.isZero
}

object DateTimeInterval {

  val zero: DateTimeInterval = DateTimeInterval(Period.ZERO, Duration.ZERO)

  def parse(str: String): Option[DateTimeInterval] = {
    val (p, d) = str.span(_ != 'T')

    val periodParsed: Option[Period] =
      \/.fromTryCatchThrowable[Period, RuntimeException](Period.parse(p)).toOption

    val durationParsed: Option[Duration] = {
      val prefix: String = if (str.startsWith("-")) "-P" else "P"

      \/.fromTryCatchThrowable[Duration, RuntimeException](Duration.parse(prefix + d.toString)).toOption
    }

    (periodParsed, durationParsed) match {
      case (Some(p), Some(d)) => DateTimeInterval(p, d).some
      case (Some(p), None) if d.isEmpty => DateTimeInterval(p, Duration.ZERO).some
      case (None, Some(d)) if p.toString === "P" || p.toString === "-P" =>
        DateTimeInterval(Period.ZERO, d).some
      case (_, _) => None
    }
  }

  object TimeLike {
    def unapply(arg: DateTimeInterval): Option[Duration] = {
      if (arg.isTimeLike) Some(arg.duration)
      else None
    }
  }

  object DateLike {
    def unapply(arg: DateTimeInterval): Option[Period] = {
      if (arg.isDateLike) Some(arg.period)
      else None
    }
  }

  def make(years: Int, months: Int, days: Int, seconds: Long, nanos: Long): DateTimeInterval =
    DateTimeInterval(Period.of(years, months, days), Duration.ofSeconds(seconds, nanos))

  def ofYears(years: Int): DateTimeInterval =
    DateTimeInterval(Period.ofYears(years), Duration.ZERO)

  def ofMonths(months: Int): DateTimeInterval =
    DateTimeInterval(Period.ofMonths(months), Duration.ZERO)

  def ofWeeks(weeks: Int): DateTimeInterval =
    DateTimeInterval(Period.ofWeeks(weeks), Duration.ZERO)

  def ofDays(days: Int): DateTimeInterval =
    DateTimeInterval(Period.ofDays(days), Duration.ZERO)

  def ofHours(hours: Long): DateTimeInterval =
    DateTimeInterval(Period.ZERO, Duration.ofHours(hours))

  def ofMinutes(minutes: Long): DateTimeInterval =
    DateTimeInterval(Period.ZERO, Duration.ofMinutes(minutes))

  def ofSeconds(seconds: Long): DateTimeInterval =
    DateTimeInterval(Period.ZERO, Duration.ofSeconds(seconds))

  def ofSecondsNanos(seconds: Long, nanos: Long): DateTimeInterval =
    DateTimeInterval(Period.ZERO, Duration.ofSeconds(seconds, nanos))

  def ofMillis(millis: Long): DateTimeInterval =
    DateTimeInterval(Period.ZERO, Duration.ofMillis(millis))

  def ofNanos(nanos: Long): DateTimeInterval =
    DateTimeInterval(Period.ZERO, Duration.ofNanos(nanos))
}
