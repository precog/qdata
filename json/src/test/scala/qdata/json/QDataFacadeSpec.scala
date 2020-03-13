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
import qdata.{QDataEncode, TestData, TestDataGenerators}

import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck
import org.specs2.mutable.SpecLike
import org.typelevel.jawn.{Facade, SupportParser}
import scalaz.syntax.std.option._

object QDataFacadeSpec extends SpecLike with ScalaCheck {
  import TestData._
  import TestDataGenerators.{genTestDataGen, genJson, genPrimitive}

  def parser[J: QDataEncode]: SupportParser[J] = new SupportParser[J] {
    implicit def facade: Facade[J] = QDataFacade[J](isPrecise = false)
  }

  implicit def arbTestData: Arbitrary[TestData] = Arbitrary(
    genTestDataGen(6, genJson(genPrimitive), genPrimitive))

  "readable json facade parsing" >> {

    "parse arbitrary" >> prop { (data0: TestData) =>
      val data: TestData = NumericRewrite.rewrite(data0)
      val json: String = TestDataRender.toJValue(data).render
      parser[TestData].parseFromString(json).toOption must_=== data.some
    }

    // a concrete example
    "parse concrete" >> {
      val json: String = """{ "foo": true, "bar": [1, null, 2.3] }"""

      parser[TestData].parseFromString(json).toOption must_===
        _Object(Map(
          ("foo", _Boolean(true)),
          ("bar", _Array(Vector(_Long(1), _Null(), _Double(2.3)))))).some
    }
  }
}
