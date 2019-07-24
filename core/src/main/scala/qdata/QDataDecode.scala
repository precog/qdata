/*
 * Copyright 2014–2019 SlamData Inc.
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
  OffsetTime,
}

trait QDataDecode[A] {

  def tpe(a: A): QType

  def getLong(a: A): Long
  def getDouble(a: A): Double
  def getReal(a: A): Real

  def getString(a: A): String
  def getBoolean(a: A): Boolean

  def getLocalDateTime(a: A): LocalDateTime
  def getLocalDate(a: A): LocalDate
  def getLocalTime(a: A): LocalTime
  def getOffsetDateTime(a: A): OffsetDateTime
  def getOffsetDate(a: A): OffsetDate
  def getOffsetTime(a: A): OffsetTime
  def getInterval(a: A): DateTimeInterval

  type ArrayCursor

  def getArrayCursor(a: A): ArrayCursor
  def hasNextArray(ac: ArrayCursor): Boolean
  def getArrayAt(ac: ArrayCursor): A
  def stepArray(ac: ArrayCursor): ArrayCursor

  type ObjectCursor

  def getObjectCursor(a: A): ObjectCursor
  def hasNextObject(ac: ObjectCursor): Boolean
  def getObjectKeyAt(ac: ObjectCursor): String
  def getObjectValueAt(ac: ObjectCursor): A
  def stepObject(ac: ObjectCursor): ObjectCursor

  def getMetaValue(a: A): A
  def getMetaMeta(a: A): A
}

object QDataDecode {
  def apply[A](implicit qd: QDataDecode[A]): QDataDecode[A] = qd
}
