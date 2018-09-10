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

import slamdata.Predef._

object QData {

  @SuppressWarnings(Array(
    "org.wartremover.warts.Recursion",
    "org.wartremover.warts.Var",
    "org.wartremover.warts.While"))
  def convert[A, B](a: A)(implicit qda: QDataDecode[A], qdb: QDataEncode[B]): B = {
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
