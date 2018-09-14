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

import jawn.{Facade, FContext}
import spire.math.Real

import java.lang.{CharSequence, NumberFormatException}

object QDataFacade {

  @SuppressWarnings(Array(
    "org.wartremover.warts.Equals",
    "org.wartremover.warts.Null",
    "org.wartremover.warts.ToString",
    "org.wartremover.warts.Var"))
  def qdata[J](implicit qd: QDataEncode[J]): Facade[J] =
    new Facade[J] {
      def jnull(): J = qd.makeNull

      def jfalse(): J = qd.makeBoolean(false)
      def jtrue(): J = qd.makeBoolean(true)

      def jnum(s: CharSequence, decIndex: Int, expIndex: Int): J =
        if (decIndex != -1) { // there is a decimal point
          // Double.parseDouble doesn't work here because it parses successfully
          // in case of a loss of precision
          // See https://gist.github.com/rintcius/49d1bfa161c53bdb733ab1a76fc19cbc
          val num = BigDecimal(s.toString) // throws NumberFormatException
          if (num.isDecimalDouble) {
            qd.makeDouble(num.doubleValue)
          } else {
            qd.makeReal(Real(s.toString)) // throws NumberFormatException
          }
        } else { // there is not a decimal point
          try {
            qd.makeLong(jawn.util.parseLong(s.toString))
          } catch {
            case _: NumberFormatException =>
              qd.makeReal(Real(s.toString)) // throws NumberFormatException
          }
        }

      def jstring(s: CharSequence): J = qd.makeString(s.toString)

      def singleContext(): FContext[J] =
        new FContext[J] {
          var result: J = _

          def add(s: CharSequence): Unit = { result = jstring(s) }
          def add(v: J): Unit = { result = v }
          def finish: J = result
          def isObj: Boolean = false
        }

      def arrayContext(): FContext[J] =
        new FContext[J] {
          var result: qd.NascentArray = qd.prepArray

          def add(s: CharSequence): Unit = { result = qd.pushArray(jstring(s), result) }
          def add(v: J): Unit = { result = qd.pushArray(v, result) }
          def finish: J = qd.makeArray(result)
          def isObj: Boolean = false
        }

      def objectContext(): FContext[J] =
        new FContext[J] {
          var result: qd.NascentObject = qd.prepObject
          var key: String = null

          def add(s: CharSequence): Unit =
            if (key == null) {
              key = s.toString
            } else {
              result = qd.pushObject(key, jstring(s), result)
              key = null
            }

          def add(v: J): Unit = {
            result = qd.pushObject(key, v, result)
            key = null
          }

          def finish: J = qd.makeObject(result)
          def isObj: Boolean = true
        }
    }
}
