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

import java.time.{
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime
}
import scala.sys.error

import scalaz.{Equal, Show}
import spire.math.Real

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
  final case class _Object(value: Map[String, TestData]) extends TestData
  final case class _Meta(value: TestData, meta: TestData) extends TestData

  ////

  implicit val equalTestData: Equal[TestData] = Equal.equalA

  implicit val showTestData: Show[TestData] = new Show[TestData] {
    override def shows(data: TestData): String = data match {
      case _Array(value) => Show.showFromToString.shows(value.toList)
      case _Object(value) => Show.showFromToString.shows(value.toList)
      case other => Show.showFromToString.shows(other)
    }
  }

  implicit def qdataEncode: QDataEncode[TestData] = QDataTestData
  implicit def qdataDecode: QDataDecode[TestData] = QDataTestData
}

private object QDataTestData extends QDataEncode[TestData] with QDataDecode[TestData] {
  import QType._
  import TestData._

  def tpe(a: TestData): QType = a match {
    case _Long(_) => QLong
    case _Double(_) => QDouble
    case _Real(_) => QReal

    case _String(_) => QString
    case _Null() => QNull
    case _Boolean(_) => QBoolean

    case _LocalDateTime(_) => QLocalDateTime
    case _LocalDate(_) => QLocalDate
    case _LocalTime(_) => QLocalTime
    case _OffsetDateTime(_) => QOffsetDateTime
    case _OffsetDate(_) => QOffsetDate
    case _OffsetTime(_) => QOffsetTime
    case _Interval(_) => QInterval

    case _Array(_) => QArray
    case _Object(_) => QObject
    case _Meta(_, _) => QMeta
  }

  def getLong(a: TestData): Long = a match {
    case _Long(v) => v
    case data => error(s"found $data, expected Long")
  }
  def makeLong(l: Long): TestData = _Long(l)

  def getDouble(a: TestData): Double = a match {
    case _Double(v) => v
    case data => error(s"found $data, expected Double")
  }
  def makeDouble(l: Double): TestData = _Double(l)

  def getReal(a: TestData): Real = a match {
    case _Real(v) => v
    case data => error(s"found $data, expected Real")
  }
  def makeReal(l: Real): TestData = _Real(l)

  def getString(a: TestData): String = a match {
    case _String(v) => v
    case data => error(s"found $data, expected String")
  }
  def makeString(l: String): TestData = _String(l)

  def makeNull: TestData = _Null()

  def getBoolean(a: TestData): Boolean = a match {
    case _Boolean(v) => v
    case data => error(s"found $data, expected Boolean")
  }
  def makeBoolean(l: Boolean): TestData = _Boolean(l)

  def getLocalDateTime(a: TestData): LocalDateTime = a match {
    case _LocalDateTime(v) => v
    case data => error(s"found $data, expected LocalDateTime")
  }
  def makeLocalDateTime(l: LocalDateTime): TestData = _LocalDateTime(l)

  def getLocalDate(a: TestData): LocalDate = a match {
    case _LocalDate(v) => v
    case data => error(s"found $data, expected LocalDate")
  }
  def makeLocalDate(l: LocalDate): TestData = _LocalDate(l)

  def getLocalTime(a: TestData): LocalTime = a match {
    case _LocalTime(v) => v
    case data => error(s"found $data, expected LocalTime")
  }
  def makeLocalTime(l: LocalTime): TestData = _LocalTime(l)

  def getOffsetDateTime(a: TestData): OffsetDateTime = a match {
    case _OffsetDateTime(v) => v
    case data => error(s"found $data, expected OffsetDateTime")
  }
  def makeOffsetDateTime(l: OffsetDateTime): TestData = _OffsetDateTime(l)

  def getOffsetDate(a: TestData): OffsetDate = a match {
    case _OffsetDate(v) => v
    case data => error(s"found $data, expected OffsetDate")
  }
  def makeOffsetDate(l: OffsetDate): TestData = _OffsetDate(l)

  def getOffsetTime(a: TestData): OffsetTime = a match {
    case _OffsetTime(v) => v
    case data => error(s"found $data, expected OffsetTime")
  }
  def makeOffsetTime(l: OffsetTime): TestData = _OffsetTime(l)

  def getInterval(a: TestData): DateTimeInterval = a match {
    case _Interval(v) => v
    case data => error(s"found $data, expected Interval")
  }
  def makeInterval(l: DateTimeInterval): TestData = _Interval(l)

  type ArrayCursor = Vector[TestData]

  def getArrayCursor(a: TestData): ArrayCursor = a match {
    case _Array(v) => v
    case data => error(s"found $data, expected Array")
  }
  def hasNextArray(ac: ArrayCursor): Boolean = !ac.isEmpty
  def getArrayAt(ac: ArrayCursor): TestData = ac.head
  def stepArray(ac: ArrayCursor): ArrayCursor = ac.tail

  type NascentArray = Vector[TestData]

  def prepArray: NascentArray = Vector[TestData]()
  def pushArray(a: TestData, na: NascentArray): NascentArray = a +: na
  def makeArray(na: NascentArray): TestData = _Array(na.reverse)

  type ObjectCursor = List[(String, TestData)]

  def getObjectCursor(a: TestData): ObjectCursor = a match {
    case _Object(v) => v.toList
    case data => error(s"found $data, expected Object")
  }
  def hasNextObject(ac: ObjectCursor): Boolean = !ac.isEmpty
  def getObjectKeyAt(ac: ObjectCursor): String = ac.head._1
  def getObjectValueAt(ac: ObjectCursor): TestData = ac.head._2
  def stepObject(ac: ObjectCursor): ObjectCursor = ac.tail

  type NascentObject = Map[String, TestData]

  def prepObject: NascentObject = Map[String, TestData]()
  def pushObject(key: String, a: TestData, na: NascentObject): NascentObject = na + ((key, a))
  def makeObject(na: NascentObject): TestData = _Object(na)

  def getMetaValue(a: TestData): TestData = a match {
    case _Meta(v, _) => v
    case data => error(s"found $data, expected Meta")
  }
  def getMetaMeta(a: TestData): TestData = a match {
    case _Meta(_, m) => m
    case data => error(s"found $data, expected Meta")
  }
  def makeMeta(value: TestData, meta: TestData): TestData = _Meta(value, meta)
}
