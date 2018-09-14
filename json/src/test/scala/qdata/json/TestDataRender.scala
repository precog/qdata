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

import qdata.TestData

import scala.collection.mutable
import scala.sys.error

import jawn.ast._
import scalaz.std.tuple._
import scalaz.syntax.functor._
import spire.math.Real.algebra

object TestDataRender {
  import PreciseKeys._
  import TestData._

  def toJValue(data: TestData): JValue =
    data match {
      case _Long(v) => LongNum(v)
      case _Double(v) => DoubleNum(v)
      case _Real(v) => DeferNum(algebra.toBigDecimal(v).toString)
      case _String(v) => JString(v)
      case _Null() => JNull
      case _Boolean(v) => JBool(v)
      case _LocalDateTime(v) => JString(v.toString)
      case _LocalDate(v) => JString(v.toString)
      case _LocalTime(v) => JString(v.toString)
      case _OffsetDateTime(v) => JString(v.toString)
      case _OffsetDate(v) => JString(v.toString)
      case _OffsetTime(v) => JString(v.toString)
      case _Interval(v) => JString(v.toString)
      case _Array(vs) => JArray(vs.map(toJValue).toArray)
      case _Object(vs) => JObject(mutable.Map(vs.map(_.map(toJValue)).toList: _*))
      case _Meta(v, m) => error("meta not supported in json")
    }

  def toPreciseJValue(data: TestData): JValue =
    data match {
      case d @ (_Long(_) | _Double(_) | _Real(_) | _String(_) | _Null() | _Boolean(_) | _Meta(_, _) ) =>
        toJValue(d)
      case _LocalDateTime(v) => JObject(mutable.Map((LocalDateTimeKey, JString(v.toString))))
      case _LocalDate(v) => JObject(mutable.Map((LocalDateKey, JString(v.toString))))
      case _LocalTime(v) => JObject(mutable.Map((LocalTimeKey, JString(v.toString))))
      case _OffsetDateTime(v) => JObject(mutable.Map((OffsetDateTimeKey, JString(v.toString))))
      case _OffsetDate(v) => JObject(mutable.Map((OffsetDateKey, JString(v.toString))))
      case _OffsetTime(v) => JObject(mutable.Map((OffsetTimeKey, JString(v.toString))))
      case _Interval(v) => JObject(mutable.Map((IntervalKey, JString(v.toString))))
      case _Array(vs) => JArray(vs.map(toPreciseJValue).toArray)
      case _Object(vs) => JObject(mutable.Map(vs.map(_.map(toPreciseJValue)).toList: _*))
    }
}
