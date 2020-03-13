/*
 * Copyright 2020 Precog Data
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

@SuppressWarnings(Array(
  "org.wartremover.warts.ToString",
  "org.wartremover.warts.Var"))
final class ArrayContext[J](implicit qd: QDataEncode[J]) extends FContext.NoIndexFContext[J] {
  private var result: qd.NascentArray = qd.prepArray

  def add(s: CharSequence): Unit =
    result = qd.pushArray(qd.makeString(s.toString), result)

  def add(v: J): Unit =
    result = qd.pushArray(v, result)

  def finish: J = qd.makeArray(result)

  val isObj: Boolean = false
}
