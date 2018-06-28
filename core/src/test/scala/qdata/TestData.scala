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

package qdata

import java.time.{
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
}
import slamdata.Predef._
import spire.math.Real
import qdata.time.{DateTimeInterval, OffsetDate}

sealed trait TestData

object TestData {
  final case class _Long(value: Long) extends TestData
  final case class _Double(value: Double) extends TestData
  final case class _Real(value: Real) extends TestData

  final case class _String(value: String) extends TestData
  final case class _Null() extends TestData
  final case class _Boolean(value: Boolean) extends TestData

  final case class _LocalDateTime(value: LocalDateTime) extends TestData
  final case class _LocalDate(value: LocalDate) extends TestData
  final case class _LocalTime(value: LocalTime) extends TestData
  final case class _OffsetDateTime(value: OffsetDateTime) extends TestData
  final case class _OffsetDate(value: OffsetDate) extends TestData
  final case class _OffsetTime(value: OffsetTime) extends TestData
  final case class _Interval(value: DateTimeInterval) extends TestData

  final case class _Array(value: Vector[TestData]) extends TestData
  final case class _Object(value: Vector[(String, TestData)]) extends TestData
  final case class _Meta(value: TestData, meta: TestData) extends TestData
}
