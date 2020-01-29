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

import org.specs2.ScalaCheck
import org.specs2.mutable.SpecLike
import slamdata.Predef.Some

object QDataTestDataSpec extends SpecLike with ScalaCheck {
  import TestDataGenerators._

  val test = QDataRoundtrip[TestData]

  "roundtrip arbitray TestData" >> prop { data: TestData =>
    test.roundtrip(data) must_=== Some(data)
  }.set(minTestsOk = 1000)
}
