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

package qdata.json

import slamdata.Predef._
import qdata.QDataEncode

import java.lang.CharSequence

import org.typelevel.jawn.FContext
import scalaz.{\/, -\/, \/-}
import scalaz.syntax.either._

@SuppressWarnings(Array(
  "org.wartremover.warts.AsInstanceOf",
  "org.wartremover.warts.Equals",
  "org.wartremover.warts.OptionPartial",
  "org.wartremover.warts.Null",
  "org.wartremover.warts.ToString",
  "org.wartremover.warts.Var"))
final class PreciseObjectContext[J](implicit qd: QDataEncode[J]) extends FContext[J] {

  private val preciseParser: PreciseParser[J] = PreciseParser[J]
  private var result: qd.NascentObject \/ J = qd.prepObject.left[J]
  private var key: String = null

  def setPrecise(str: String): Unit = {
    key = str
    result = null.asInstanceOf[qd.NascentObject \/ J]
  }

  def add(s: CharSequence): Unit = {
    if (key == null) { // we are parsing a key
      val str = s.toString

      if (preciseParser.isPreciseKey(str))
        setPrecise(str)
      else
        key = str

    } else if (result == null) { // we are parsing a precise value
      result = preciseParser.parsePrecise(key, s.toString).right[qd.NascentObject]
      key = null
    } else { // we are parsing a non-precise value
      val newResult: qd.NascentObject = result match {
        case -\/(obj) => obj
        case \/-(_) => scala.sys.error("nope")
      }
      result = qd.pushObject(key, qd.makeString(s.toString), newResult).left[J]
      key = null
    }
  }

  def add(v: J): Unit = {
    val newResult: qd.NascentObject = result match {
      case -\/(obj) => obj
      case \/-(_) => scala.sys.error("nope")
    }
    result = qd.pushObject(key, v, newResult).left[J]
    key = null
  }

  def finish: J = result match {
    case -\/(r) => qd.makeObject(r)
    case \/-(j) => j
  }

  val isObj: Boolean = true
}
