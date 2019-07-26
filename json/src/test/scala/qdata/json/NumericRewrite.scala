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

import qdata.TestData

import scalaz.std.tuple._
import scalaz.syntax.functor._
import spire.math.Real.algebra

object NumericRewrite {

  // rewrites numerics so that they roundtrip
  def rewrite(data: TestData): TestData = data match {
    case d @ TestData._Real(v) =>
      val bd = algebra.toBigDecimal(v)

      if (bd.isValidLong)
        TestData._Long(algebra.toLong(v))
      else if (bd.isDecimalDouble)
        TestData._Double(algebra.toDouble(v))
      else
        TestData._Real(bd)

    case TestData._Object(vs) => TestData._Object(vs.map(_.map(rewrite(_))))
    case TestData._Array(vs) => TestData._Array(vs.map(rewrite(_)))
    case TestData._Meta(v, m) => TestData._Meta(rewrite(v), rewrite(m))

    case other => other
  }
}
