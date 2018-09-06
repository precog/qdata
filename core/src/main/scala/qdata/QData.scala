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
import qdata.time.{DateTimeInterval, OffsetDate}
import slamdata.Predef._
import spire.math.Real


trait QData[A] {
  def tpe(a: A): QType

  def getLong(a: A): Long
  def makeLong(l: Long): A

  def getDouble(a: A): Double
  def makeDouble(l: Double): A

  def getReal(a: A): Real
  def makeReal(l: Real): A

  def getString(a: A): String
  def makeString(l: String): A

  def makeNull: A

  def getBoolean(a: A): Boolean
  def makeBoolean(l: Boolean): A

  def getLocalDateTime(a: A): LocalDateTime
  def makeLocalDateTime(l: LocalDateTime): A

  def getLocalDate(a: A): LocalDate
  def makeLocalDate(l: LocalDate): A

  def getLocalTime(a: A): LocalTime
  def makeLocalTime(l: LocalTime): A

  def getOffsetDateTime(a: A): OffsetDateTime
  def makeOffsetDateTime(l: OffsetDateTime): A

  def getOffsetDate(a: A): OffsetDate
  def makeOffsetDate(l: OffsetDate): A

  def getOffsetTime(a: A): OffsetTime
  def makeOffsetTime(l: OffsetTime): A

  def getInterval(a: A): DateTimeInterval
  def makeInterval(l: DateTimeInterval): A

  type ArrayCursor

  def getArrayCursor(a: A): ArrayCursor
  def hasNextArray(ac: ArrayCursor): Boolean
  def getArrayAt(ac: ArrayCursor): A
  def stepArray(ac: ArrayCursor): ArrayCursor

  type NascentArray

  def prepArray: NascentArray
  def pushArray(a: A, na: NascentArray): NascentArray
  def makeArray(na: NascentArray): A

  type ObjectCursor

  def getObjectCursor(a: A): ObjectCursor
  def hasNextObject(ac: ObjectCursor): Boolean
  def getObjectKeyAt(ac: ObjectCursor): String
  def getObjectValueAt(ac: ObjectCursor): A
  def stepObject(ac: ObjectCursor): ObjectCursor

  type NascentObject

  def prepObject: NascentObject
  def pushObject(key: String, a: A, na: NascentObject): NascentObject
  def makeObject(na: NascentObject): A

  def getMetaValue(a: A): A
  def getMetaMeta(a: A): A
  def makeMeta(value: A, meta: A): A
}

object QData {
  def apply[A](implicit qd: QData[A]): QData[A] = qd

  @SuppressWarnings(Array(
    "org.wartremover.warts.Recursion",
    "org.wartremover.warts.Var",
    "org.wartremover.warts.While"))
  def convert[A, B](a: A)(implicit qda: QData[A], qdb: QData[B]): B = {
    import QType._

    qda.tpe(a) match {
      case QLong => qdb.makeLong(qda.getLong(a))
      case QDouble => qdb.makeDouble(qda.getDouble(a))
      case QReal => qdb.makeReal(qda.getReal(a))
      case QString => qdb.makeString(qda.getString(a))
      case QNull => qdb.makeNull
      case QBoolean => qdb.makeBoolean(qda.getBoolean(a))
      case QLocalDateTime => qdb.makeLocalDateTime(qda.getLocalDateTime(a))
      case QLocalDate => qdb.makeLocalDate(qda.getLocalDate(a))
      case QLocalTime => qdb.makeLocalTime(qda.getLocalTime(a))
      case QOffsetDateTime => qdb.makeOffsetDateTime(qda.getOffsetDateTime(a))
      case QOffsetDate => qdb.makeOffsetDate(qda.getOffsetDate(a))
      case QOffsetTime => qdb.makeOffsetTime(qda.getOffsetTime(a))
      case QInterval => qdb.makeInterval(qda.getInterval(a))

      case QObject => {
        var cursor = qda.getObjectCursor(a)
        var nascent = qdb.prepObject

        while (qda.hasNextObject(cursor)) {
          nascent = qdb.pushObject(
            qda.getObjectKeyAt(cursor),
            convert[A, B](qda.getObjectValueAt(cursor)),
            nascent)
          cursor = qda.stepObject(cursor)
        }

        qdb.makeObject(nascent)
      }

      case QArray => {
        var cursor = qda.getArrayCursor(a)
        var nascent = qdb.prepArray

        while (qda.hasNextArray(cursor)) {
          nascent = qdb.pushArray(convert[A, B](qda.getArrayAt(cursor)), nascent)
          cursor = qda.stepArray(cursor)
        }

        qdb.makeArray(nascent)
      }

      case QMeta => qdb.makeMeta(
        convert[A, B](qda.getMetaValue(a)),
        convert[A, B](qda.getMetaMeta(a)))
    }
  }
}
