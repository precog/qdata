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

import slamdata.Predef.{Array, BigDecimal, Int, String, SuppressWarnings}

import qdata.QDataEncode

import java.lang.NumberFormatException

import org.typelevel.jawn.util.parseLong
import spire.math.Real

final class NumericParser[A] private (implicit A: QDataEncode[A]) {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  def parseNumber(s: String, decIndex: Int, expIndex: Int): A =
    if (decIndex != -1) { // there is a decimal point
      // Double.parseDouble doesn't work here because it parses successfully
      // in case of a loss of precision
      // See https://gist.github.com/rintcius/49d1bfa161c53bdb733ab1a76fc19cbc
      val num = BigDecimal(s) // throws NumberFormatException
      if (num.isDecimalDouble) {
        A.makeDouble(num.doubleValue)
      } else {
        A.makeReal(Real(s)) // throws NumberFormatException
      }
    } else { // there is not a decimal point
      try {
        A.makeLong(parseLong(s))
      } catch {
        case _: NumberFormatException =>
          A.makeReal(Real(s)) // throws NumberFormatException
      }
    }
}

object NumericParser {
  def apply[A: QDataEncode]: NumericParser[A] =
    new NumericParser[A]
}
