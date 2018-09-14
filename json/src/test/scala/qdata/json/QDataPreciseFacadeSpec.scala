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
import qdata.{QDataEncode, TestData, TestDataGenerators}

import jawn.{Facade, SupportParser}
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.SpecLike
import scalaz.syntax.std.option._

object QDataPreciseFacadeSpec extends SpecLike with ScalaCheck {
  import TestData._
  import TestDataGenerators.{genTestDataGen, genJson, genNonNested}

  def parser[J: QDataEncode]: SupportParser[J] = new SupportParser[J] {
    implicit def facade: Facade[J] = QDataPreciseFacade.qdataPrecise[J]
  }

  implicit def arbTestData: Arbitrary[TestData] = Arbitrary(
    genTestDataGen(6, genJson(genNonNested), genNonNested))

  "precise json facade parsing" >> {

    "parse arbitrary" >> prop { (data0: TestData) =>
      val data: TestData = NumericRewrite.rewrite(data0)
      val json: String = TestDataRender.toPreciseJValue(data).render
      parser[TestData].parseFromString(json).toOption must_=== data.some
    }

    // a concrete example
    "parse concrete" >> {
      val json: String = """{ "foo": true, "bar": { "$localtime": "12:34" } }"""

      parser[TestData].parseFromString(json).toOption must_===
        _Object(Map(
          ("foo", _Boolean(true)),
          ("bar", _LocalTime(java.time.LocalTime.parse("12:34"))))).some
    }
  }
}
