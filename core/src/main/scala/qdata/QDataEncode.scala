/*
 * Copyright 2014–2020 SlamData Inc.
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

import slamdata.Predef._

import qdata.time.{DateTimeInterval, OffsetDate}

import spire.math.Real

import java.time.{
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime
}

trait QDataEncode[A] {

  def makeLong(l: Long): A
  def makeDouble(l: Double): A
  def makeReal(l: Real): A

  def makeString(l: String): A
  def makeNull: A
  def makeBoolean(l: Boolean): A

  def makeLocalDateTime(l: LocalDateTime): A
  def makeLocalDate(l: LocalDate): A
  def makeLocalTime(l: LocalTime): A
  def makeOffsetDateTime(l: OffsetDateTime): A
  def makeOffsetDate(l: OffsetDate): A
  def makeOffsetTime(l: OffsetTime): A
  def makeInterval(l: DateTimeInterval): A

  type NascentArray

  def prepArray: NascentArray
  def pushArray(a: A, na: NascentArray): NascentArray
  def makeArray(na: NascentArray): A

  type NascentObject

  def prepObject: NascentObject
  def pushObject(key: String, a: A, na: NascentObject): NascentObject
  def makeObject(na: NascentObject): A

  def makeMeta(value: A, meta: A): A
}

object QDataEncode {
  def apply[A](implicit qd: QDataEncode[A]): QDataEncode[A] = qd
}
