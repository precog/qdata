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

import scala.annotation.tailrec
import scalaz.\/
import slamdata.Predef._

final class QDataRoundtrip[A](implicit qdata: QData[A]) {
  import qdata._
  import QType._

  def roundtripUnsafe(data: A): A =
    tpe(data) match {
      case QNull => makeNull
      case QString => makeString(getString(data))
      case QBoolean => makeBoolean(getBoolean(data))
      case QReal => makeReal(getReal(data))
      case QDouble => makeDouble(getDouble(data))
      case QLong => makeLong(getLong(data))
      case QOffsetDateTime => makeOffsetDateTime(getOffsetDateTime(data))
      case QOffsetDate => makeOffsetDate(getOffsetDate(data))
      case QOffsetTime => makeOffsetTime(getOffsetTime(data))
      case QLocalDateTime => makeLocalDateTime(getLocalDateTime(data))
      case QLocalDate => makeLocalDate(getLocalDate(data))
      case QLocalTime => makeLocalTime(getLocalTime(data))
      case QInterval => makeInterval(getInterval(data))
      case QArray => makeArray(getArray(data))
      case QObject => makeObject(getObject(data))
      case QMeta => makeMeta(getMetaValue(data), getMetaMeta(data))
    }

  def roundtrip(data: A): Option[A] =
    \/.fromTryCatchNonFatal(roundtripUnsafe(data)).toOption

  ////

  private def getArray(a: A): NascentArray = {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    @tailrec
    def iterate(cursor: ArrayCursor, nascent: NascentArray): NascentArray =
      if (hasNextArray(cursor))
        iterate(stepArray(cursor), pushArray(getArrayAt(cursor), nascent))
      else
        nascent

    iterate(getArrayCursor(a), prepArray)
  }

  private def getObject(a: A): NascentObject = {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    @tailrec
    def iterate(cursor: ObjectCursor, nascent: NascentObject): NascentObject =
      if (hasNextObject(cursor))
        iterate(
         stepObject(cursor),
         pushObject(getObjectKeyAt(cursor), getObjectValueAt(cursor), nascent))
      else
        nascent

    iterate(getObjectCursor(a), prepObject)
  }
}

object QDataRoundtrip {
  def apply[A: QData]: QDataRoundtrip[A] =
    new QDataRoundtrip[A]
}
