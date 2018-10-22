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

package qdata.json

import slamdata.Predef.{Array, Boolean, String, SuppressWarnings}
import qdata.QDataEncode
import qdata.time.{DateTimeInterval, OffsetDate}

import java.time.{
  LocalDateTime,
  LocalDate,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
}

final class PreciseParser[A] private (implicit A: QDataEncode[A]) {
  import PreciseKeys._

  def isPreciseKey(key: String): Boolean =
    PreciseKeys.All.contains(key)

  @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
  def parsePrecise(key: String, value: String): A =
    key match {
      case `OffsetDateTimeKey` =>
        A.makeOffsetDateTime(OffsetDateTime.parse(value))
      case `OffsetDateKey` =>
        A.makeOffsetDate(OffsetDate.parse(value))
      case `OffsetTimeKey` =>
        A.makeOffsetTime(OffsetTime.parse(value))
      case `LocalDateTimeKey` =>
        A.makeLocalDateTime(LocalDateTime.parse(value))
      case `LocalDateKey` =>
        A.makeLocalDate(LocalDate.parse(value))
      case `LocalTimeKey` =>
        A.makeLocalTime(LocalTime.parse(value))
      case `IntervalKey` =>
        A.makeInterval(DateTimeInterval.parse(value).get)
      case other =>
        scala.sys.error("parsePrecise(): Unrecognized key: " + other)
    }
}

object PreciseParser {
  def apply[A: QDataEncode]: PreciseParser[A] =
    new PreciseParser[A]
}
