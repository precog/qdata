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

package qdata.tectonic

import slamdata.Predef.{Array, Int, String, SuppressWarnings}

import qdata.{QDataDecode, QType}
import qdata.json.PreciseKeys

import tectonic.{Plate, Signal}

import scala.util.control.NonFatal
import scala.util.{Either, Left, Right}

import scalaz.Foldable

// TODO: Signal handling
@SuppressWarnings(Array(
  "org.wartremover.warts.NonUnitStatements",
  "org.wartremover.warts.Recursion",
  "org.wartremover.warts.Var",
  "org.wartremover.warts.While"))
final class QDataDriver[A] private (plate: Plate[A]) {
  import PreciseKeys._
  import QType._

  def consume[F[_], D](
      input: F[D])(
      implicit F: Foldable[F], D: QDataDecode[D])
      : Either[DriverException, A] = {

    def drivePreciseValue(k: String, v: String): Signal  = {
      plate.nestMap(k)
      plate.str(v)
      plate.unnest()
    }

    def expIndex(s: String): Int = {
      val e = s.indexOf('e')
      if (e < 0) s.indexOf('E') else e
    }

    def drive(d: D): Signal =
      D.tpe(d) match {
        case QLong =>
          plate.num(D.getLong(d).toString, -1, -1)

        case QDouble =>
          val s = D.getDouble(d).toString
          plate.num(s, s.indexOf('.'), expIndex(s))

        case QReal =>
          val s = D.getReal(d).getString(Int.MaxValue)
          plate.num(s, s.indexOf('.'), expIndex(s))

        case QString =>
          plate.str(D.getString(d))

        case QNull =>
          plate.nul()

        case QBoolean =>
          if (D.getBoolean(d)) plate.tru() else plate.fls()

        case QLocalDateTime =>
          drivePreciseValue(LocalDateTimeKey, D.getLocalDateTime(d).toString)

        case QLocalDate =>
          drivePreciseValue(LocalDateKey, D.getLocalDate(d).toString)

        case QLocalTime =>
          drivePreciseValue(LocalTimeKey, D.getLocalTime(d).toString)

        case QOffsetDateTime =>
          drivePreciseValue(OffsetDateTimeKey, D.getOffsetDateTime(d).toString)

        case QOffsetDate =>
          drivePreciseValue(OffsetDateKey, D.getOffsetDate(d).toString)

        case QOffsetTime =>
          drivePreciseValue(OffsetTimeKey, D.getOffsetTime(d).toString)

        case QInterval =>
          drivePreciseValue(IntervalKey, D.getInterval(d).toString)

        case QArray =>
          var acur = D.getArrayCursor(d)

          if (D.hasNextArray(acur)) {
            while (D.hasNextArray(acur)) {
              plate.nestArr()
              drive(D.getArrayAt(acur))
              plate.unnest()
              acur = D.stepArray(acur)
            }
            Signal.Continue // TODO: handle signals in loop
          } else {
            plate.arr()
          }

        case QObject =>
          var ocur = D.getObjectCursor(d)

          if (D.hasNextObject(ocur)) {
            while (D.hasNextObject(ocur)) {
              plate.nestMap(D.getObjectKeyAt(ocur))
              drive(D.getObjectValueAt(ocur))
              plate.unnest()
              ocur = D.stepObject(ocur)
            }
            Signal.Continue // TODO: handle signals in loop
          } else {
            plate.map()
          }

        case QMeta =>
          drive(D.getMetaValue(d))

          val meta = D.getMetaMeta(d)

          D.tpe(meta) match {
            case QObject =>
              var ocur = D.getObjectCursor(meta)

              while (D.hasNextObject(ocur)) {
                plate.nestMeta(D.getObjectKeyAt(ocur))
                drive(D.getObjectValueAt(ocur))
                plate.unnest()
                ocur = D.stepObject(ocur)
              }

              Signal.Continue // TODO: handle signals in loop

            case other => Signal.Continue
          }
      }

      try {
        F.foldLeft(input, ()) { (_, d) =>
          drive(d)
          plate.finishRow()
        }

        Right(plate.finishBatch(false))
      } catch {
        case NonFatal(t) => Left(new DriverException(t))
      }
  }

  def finish(): Either[DriverException, A] =
    try {
      Right(plate.finishBatch(true))
    } catch {
        case NonFatal(t) => Left(new DriverException(t))
    }
}

object QDataDriver {
  def apply[A](plate: Plate[A]): QDataDriver[A] =
    new QDataDriver(plate)
}
