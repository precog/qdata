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

import java.lang.Math
import java.time.{
  Duration,
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
  Period
}
import java.util.regex.Pattern

import scala.util.Try

import scalaz.Monoid
import scalaz.syntax.equal._
import scalaz.std.anyVal._

// ISO8601 has the conviction that an interval of "25 hours" is invalid.
// I don't think that's reasonable. There's no guarantee that 25 hours construed as
// a measure of seconds is equal to one day and one hour.
// note that parse->toString is not an isomorphism, because the first minus sign
// is absorbed into the rest of the units in the interval,
// and nanos in excess of a second are added into seconds
//
// Let it be known: adding months does not behave as a monoid action on +.
// LocalDate.of(1, 1, 31).plusMonths(2) == LocalDate.of(1, 3, 31)
// LocalDate.of(1, 1, 31).plusMonths(1).plusMonths(1) == LocalDate.of(1, 3, 28)

@SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
final class DateTimeInterval private(
                                val years: Int,
                                val months: Int,
                                val days: Int,
                                val seconds: Long,
                                val nanos: Int
                              ) {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: DateTimeInterval =>
      seconds == other.seconds && nanos == other.nanos &&
        years == other.years && months == other.months && days == other.days
    case _ => false
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  override def hashCode(): Int = {
    var result = 1
    result = (31 * result) + years
    result = (31 * result) + months
    result = (31 * result) + days
    result = (31 * result) + java.lang.Long.hashCode(seconds)
    result = (31 * result) + nanos
    result
  }

  def plus(other: DateTimeInterval) =
    DateTimeInterval(years + other.years, months + other.months, days + other.days,
      seconds + other.seconds, nanos.toLong + other.nanos.toLong)

  def minus(other: DateTimeInterval) =
    DateTimeInterval(years - other.years, months - other.months, days - other.days,
      seconds - other.seconds, nanos.toLong - other.nanos.toLong)

  def multiply(factor: Int): DateTimeInterval =
    DateTimeInterval(years * factor, months * factor, days * factor,
      seconds * factor, nanos.toLong * factor)

  @SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.While"))
  override def toString: String =
    if (years == 0 && months == 0 && days == 0 && seconds == 0 && nanos == 0) {
      "P0D"
    } else {
      val buf = new StringBuilder()
      buf.append('P')
      if (years != 0) {
        buf.append(years).append('Y')
      }
      if (months != 0) {
        buf.append(months).append('M')
      }
      if (days != 0) {
        buf.append(days).append('D')
      }
      if (seconds != 0 || nanos != 0) {
        buf.append('T')
        val hours = seconds / 3600L
        val minutes = ((seconds % 3600) / 60).toInt
        val secs = seconds % 60
        if (hours != 0) {
          buf.append(hours).append('H')
        }
        if (minutes != 0) {
          buf.append(minutes).append('M')
        }
        if (secs != 0 || nanos != 0 || buf.length() <= 2) {
          if (secs < 0 && nanos > 0) {
            if (secs == -1) {
              buf.append("-0")
            } else {
              buf.append(secs + 1)
            }
          } else {
            buf.append(secs)
          }
          if (nanos > 0) {
            val pos = buf.length()
            if (secs < 0) {
              buf.append(2 * 1000000000L - nanos)
            } else {
              buf.append(nanos + 1000000000L)
            }
            while (buf.charAt(buf.length() - 1) == '0') {
              buf.setLength(buf.length() - 1)
            }
            buf.setCharAt(pos, '.')
          }
          buf.append('S')
        }
      }
      buf.toString
    }

  def compareTo(other: DateTimeInterval): Int =
    toString.compareTo(other.toString)

  def subtractFromLocalDateTime(dt: LocalDateTime): LocalDateTime =
    dt.minus(this.toPeriod).plus(this.toDuration)

  def subtractFromLocalDate(dt: LocalDate): LocalDate =
    dt.minus(this.toPeriod)

  def subtractFromLocalTime(dt: LocalTime): LocalTime =
    dt.minus(this.toDuration)

  def subtractFromOffsetDateTime(odt: OffsetDateTime): OffsetDateTime =
    OffsetDateTime.of(subtractFromLocalDateTime(odt.toLocalDateTime), odt.getOffset)

  def subtractFromOffsetDate(odt: OffsetDate): OffsetDate =
    OffsetDate(subtractFromLocalDate(odt.date), odt.offset)

  def subtractFromOffsetTime(odt: OffsetTime): OffsetTime =
    OffsetTime.of(subtractFromLocalTime(odt.toLocalTime), odt.getOffset)

  def addToLocalDateTime(dt: LocalDateTime): LocalDateTime =
    dt.plus(this.toPeriod).plus(this.toDuration)

  def addToLocalDate(dt: LocalDate): LocalDate =
    dt.plus(this.toPeriod)

  def addToLocalTime(dt: LocalTime): LocalTime =
    dt.plus(this.toDuration)

  def addToOffsetDateTime(odt: OffsetDateTime): OffsetDateTime =
    OffsetDateTime.of(addToLocalDateTime(odt.toLocalDateTime), odt.getOffset)

  def addToOffsetDate(odt: OffsetDate): OffsetDate =
    OffsetDate(addToLocalDate(odt.date), odt.offset)

  def addToOffsetTime(odt: OffsetTime): OffsetTime =
    OffsetTime.of(addToLocalTime(odt.toLocalTime), odt.getOffset)

  def isDateLike: Boolean = seconds ≟ 0 && nanos ≟ 0
  def isTimeLike: Boolean = years ≟ 0 && months ≟ 0 && days ≟ 0
  def toDuration: Duration = Duration.ofSeconds(seconds, nanos.toLong)
  def toPeriod: Period = Period.of(years, months, days)
}

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
object DateTimeInterval {

  val PATTERN: Pattern =
    Pattern.compile("([-+]?)P(?:([-+]?[0-9]+)Y)?(?:([-+]?[0-9]+)M)?(?:([-+]?[0-9]+)W)?(?:([-+]?[0-9]+)D)?" +
      "(T(?:([-+]?[0-9]+)H)?(?:([-+]?[0-9]+)M)?(?:([-+]?[0-9]+)(?:[.,]([0-9]{0,9}))?S)?)?",
      Pattern.CASE_INSENSITIVE)

  val zero: DateTimeInterval = new DateTimeInterval(0, 0, 0, 0L, 0)

  object TimeLike {
    def unapply(arg: DateTimeInterval): Option[Duration] = {
      if (arg.isTimeLike) Some(arg.toDuration)
      else None
    }
  }

  object DateLike {
    def unapply(arg: DateTimeInterval): Option[Period] = {
      if (arg.isDateLike) Some(arg.toPeriod)
      else None
    }
  }

  def makeUnsafe(years: Int,
                 months: Int,
                 days: Int,
                 seconds: Long,
                 nanos: Int
                ) = new DateTimeInterval(years, months, days, seconds, nanos)

  implicit val dateTimeIntervalMonoid: Monoid[DateTimeInterval] = new Monoid[DateTimeInterval] {
    val zero = DateTimeInterval.zero
    def append(f1: DateTimeInterval, f2: => DateTimeInterval) = f1.plus(f2)
  }

  def apply(years: Int, months: Int, days: Int, seconds: Long, nanos: Long): DateTimeInterval = {
    if ((years | months | days | seconds | nanos) == 0) zero
    else {
      val secs = java.lang.Math.addExact(seconds, java.lang.Math.floorDiv(nanos, 1000000000))
      val nos = java.lang.Math.floorMod(nanos, 1000000000).toInt
      new DateTimeInterval(years, months, days, secs, nos)
    }
  }

  def ofYears(years: Int): DateTimeInterval = new DateTimeInterval(years, 0, 0, 0, 0)
  def ofMonths(months: Int): DateTimeInterval = new DateTimeInterval(0, months, 0, 0, 0)
  def ofWeeks(weeks: Int): DateTimeInterval = new DateTimeInterval(0, 0, weeks * 7, 0, 0)
  def ofDays(days: Int): DateTimeInterval = new DateTimeInterval(0, 0, days, 0, 0)
  def ofHours(hours: Long): DateTimeInterval = create(hours * 3600, 0)
  def ofMinutes(mins: Long): DateTimeInterval = create(mins * 60, 0)
  def ofSeconds(secs: Long): DateTimeInterval = create(secs, 0)
  def ofSecondsNanos(seconds: Long, nanos: Long): DateTimeInterval = apply(0, 0, 0, seconds, nanos)
  def ofMillis(millis: Long): DateTimeInterval = create(millis / 1000, ((millis % 1000) * 1000000).toInt)
  def ofNanos(nanos: Long): DateTimeInterval = DateTimeInterval(0, 0, 0, 0L, nanos)

  private def create(seconds: Long, nanos: Int): DateTimeInterval = {
    if ((seconds | nanos) == 0) zero
    else new DateTimeInterval(0, 0, 0, seconds, nanos)
  }

  def parse(str: String): Option[DateTimeInterval] =
    for {
      matcher <- Some(PATTERN.matcher(str))
      if matcher.matches() && matcher.group(6) != "T" &&
      ((matcher.group(2) ne null) || (matcher.group(3) ne null) || (matcher.group(4) ne null) || (matcher.group(5) ne null) || 
        (matcher.group(7) ne null) || (matcher.group(8) ne null) || (matcher.group(9) ne null))
      negated = if (matcher.group(1) == "-") -1 else 1
      years <- parseIntNullable(matcher.group(2))
      months <- parseIntNullable(matcher.group(3))
      weeks <- parseIntNullable(matcher.group(4))
      days <- parseIntNullable(matcher.group(5))
      hours <- parseIntNullable(matcher.group(7))
      minutes <- parseIntNullable(matcher.group(8))
      seconds <- parseLongNullable(matcher.group(9))
      nanos <- {
        if (matcher.group(10) eq null) Some(0L)
        // negative fractions seem invalid to me. `0.-10`?
        else if (matcher.group(10).length > 9 || matcher.group(10).charAt(0) == '-') None
        else Try((matcher.group(10) + "000000000").substring(0, 9).toLong).toOption
      }
      if nanos >= 0
    } yield DateTimeInterval(
        years = years * negated,
        months = months * negated,
        days = (days + weeks * 7) * negated,
        // no overflow handling here. Is it needed?
        seconds = (seconds + minutes * 60L + hours * 60L * 60L) * negated,
        nanos = if ((matcher.group(9) == "-0") || seconds < 0 || negated == -1) -nanos else nanos
      )

  def parseIntNullable(str: String): Option[Int] =
    if (str eq null) Some(0)
    else Try(str.toInt).toOption

  def parseLongNullable(str: String): Option[Long] =
    if (str eq null) Some(0L)
    else Try(str.toLong).toOption

  def abs(interval: DateTimeInterval): DateTimeInterval =
    DateTimeInterval(
      Math.abs(interval.years),
      Math.abs(interval.months),
      Math.abs(interval.days),
      Math.abs(interval.seconds),
      Math.abs(interval.nanos).toLong)

  def divideBy(interval: DateTimeInterval, divisor: Int) = {
    DateTimeInterval(
      interval.years / divisor,
      interval.months / divisor,
      interval.days / divisor,
      interval.seconds / divisor,
      (interval.nanos / divisor).toLong)
  }

  def divideByDouble(interval: DateTimeInterval, divisor: Double) = {
    DateTimeInterval(
      interval.years / divisor.round.toInt,
      interval.months / divisor.round.toInt,
      interval.days / divisor.round.toInt,
      interval.seconds / divisor.round,
      interval.nanos / divisor.round)
  }
}
