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

import slamdata.Predef._
import qdata.QDataEncode

import java.lang.CharSequence

import jawn.FContext

@SuppressWarnings(Array(
  "org.wartremover.warts.Equals",
  "org.wartremover.warts.Null",
  "org.wartremover.warts.ToString",
  "org.wartremover.warts.Var"))
final class ObjectContext[J](implicit qd: QDataEncode[J]) extends FContext[J] {
  private var result: qd.NascentObject = qd.prepObject
  private var key: String = null

  def add(s: CharSequence): Unit =
    if (key == null) {
      key = s.toString
    } else {
      result = qd.pushObject(key, qd.makeString(s.toString), result)
      key = null
    }

  def add(v: J): Unit = {
    result = qd.pushObject(key, v, result)
    key = null
  }

  def finish: J = qd.makeObject(result)

  val isObj: Boolean = true
}
