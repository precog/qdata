/*
 * Copyright 2020 Precog Data
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

package qdata.time


import java.time.{
  Duration,
  LocalDate => JLocalDate,
  LocalDateTime => JLocalDateTime,
  LocalTime => JLocalTime,
  OffsetDateTime => JOffsetDateTime,
  OffsetTime => JOffsetTime,
  Period,
  ZoneOffset
}
import java.time.temporal.ChronoField
import org.scalacheck.{Arbitrary, Gen}
import scala.math
import slamdata.Predef._

object TimeGenerators {

  // interval
  implicit val arbDuration: Arbitrary[Duration] = genDuration
  implicit val arbPeriod: Arbitrary[Period] = genPeriod
  implicit val arbDateTimeInterval: Arbitrary[DateTimeInterval] = genInterval

  // local
  implicit val arbLocalDate: Arbitrary[JLocalDate] = genLocalDate
  implicit val arbLocalTime: Arbitrary[JLocalTime] = genLocalTime
  implicit val arbLocalDateTime: Arbitrary[JLocalDateTime] = genLocalDateTime

  // offset
  implicit val arbOffsetDate: Arbitrary[OffsetDate] = genOffsetDate
  implicit val arbOffsetTime: Arbitrary[JOffsetTime] = genOffsetTime
  implicit val arbOffsetDateTime: Arbitrary[JOffsetDateTime] = genOffsetDateTime
  implicit val arbZoneOffset: Arbitrary[ZoneOffset] = genZoneOffset

  ////

  // FIXME
  // only generate positive seconds until this is available (Java 9)
  // https://bugs.openjdk.java.net/browse/JDK-8054978
  def genDuration: Gen[Duration] =
    for {
      seconds <- genSeconds
      nanos <- genNanos
    } yield Duration.ofSeconds(math.abs(seconds.toLong), math.abs(nanos))

  def genPeriod: Gen[Period] =
    for {
      years <- genYears
      months <- genMonths
      days <- genDays
    } yield Period.of(years, months, days)

  // FIXME
  // only generate positive seconds until this is available (Java 9)
  // https://bugs.openjdk.java.net/browse/JDK-8054978
  def genDateTimeInterval: Gen[DateTimeInterval] =
    for {
      years <- genYears
      months <- genMonths
      days <- genDays
      seconds <- genSeconds
      nanos <- genNanos
    } yield DateTimeInterval.make(years, months, days, math.abs(seconds.toLong), math.abs(nanos))

  def genDateInterval: Gen[DateTimeInterval] =
    genPeriod.map(DateTimeInterval.ofPeriod)

  def genTimeInterval: Gen[DateTimeInterval] =
    genDuration.map(DateTimeInterval.ofDuration)

  def genInterval: Gen[DateTimeInterval] =
    Gen.oneOf(genDateTimeInterval, genTimeInterval, genDateInterval)

  def genLocalDate: Gen[JLocalDate] =
    Gen.choose[Long](minLocalEpochDay, maxLocalEpochDay).map(JLocalDate.ofEpochDay)

  def genLocalTime: Gen[JLocalTime] =
    Gen.choose[Long](ChronoField.NANO_OF_DAY.range().getLargestMinimum, ChronoField.NANO_OF_DAY.range().getMaximum).map(JLocalTime.ofNanoOfDay)

  def genLocalDateTime: Gen[JLocalDateTime] =
    for {
      time <- genLocalTime
      date <- genLocalDate
    } yield JLocalDateTime.of(date, time)

  def genOffsetDate: Gen[OffsetDate] = for {
    date <- genLocalDate
    zo <- genZoneOffset
  } yield OffsetDate(date, zo)

  def genOffsetTime: Gen[JOffsetTime] = for {
    time <- genLocalTime
    zo <- genZoneOffset
  } yield JOffsetTime.of(time, zo)

  def genOffsetDateTime: Gen[JOffsetDateTime] = for {
    datetime <- genLocalDateTime
    zo <- genZoneOffset
  } yield JOffsetDateTime.of(datetime, zo)

  def genZoneOffset: Gen[ZoneOffset] =
    Gen.choose[Int](ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)

  ////

  private def genSeconds: Gen[Long] = Gen.choose(Int.MinValue, Int.MaxValue) map (_.toLong)
  private def genNanos: Gen[Long] = Gen.choose(0L, 999999999L)
  private def genYears: Gen[Int] = Gen.choose(-9999999, 9999999)
  private def genMonths: Gen[Int] = Gen.choose(-100, 100)
  private def genDays: Gen[Int] = Gen.choose(-1000, 1000)

  // these are adjusted so that LocalDate.ofEpochDay(minLocalEpochDay) - genYears can't go below the
  // minimum year of LocalDate, vice versa for maxLocalEpochDay
  private val minLocalEpochDay: Long = 365L * -9999999
  private val maxLocalEpochDay: Long = 365L * 9999999

  private implicit def liftGenerator[A](g: Gen[A]): Arbitrary[A] = Arbitrary(g)
}
