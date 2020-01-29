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

package qdata.tectonic

import slamdata.Predef.{Int, List, Map}

import cats.effect.IO

import qdata.{TestData, TestDataGenerators}

import scala.Predef.$conforms
import scala.math.{abs, max}

import org.specs2.ScalaCheck
import org.specs2.mutable.SpecLike

import scalaz.std.either._
import scalaz.std.list._
import scalaz.syntax.foldable._

object TectonicSupportSpec extends SpecLike with ScalaCheck {
  import TestData._
  import TestDataGenerators._

  /** Rewrites `TestData` to be compatible with `Plate`. */
  def normalize(td: TestData): TestData =
    td match {
      case _Array(xs) =>
        _Array(xs map normalize)

      case _Object(xs) =>
        _Object(xs map { case (k, v) => (k, normalize(v)) })

      case _Meta(_Meta(v, mi), mo) =>
        normalize(_Meta(v, _Meta(mi, mo)))

      case _Meta(v, m) =>
        normalize(m) match {
          // Plate cannot represent "empty" metadata.
          case _Object(xs) if xs.isEmpty => normalize(v)
          case o @ _Object(_) => _Meta(normalize(v), o)
          // Plate cannot represent non-object metadata
          case other => _Meta(normalize(v), _Object(Map(("meta", other))))
        }

      // Sidesteps string representation issues for Real.
      case _Real(r) =>
        _Long(r.toLong)

      case other => other
    }

  "roundtrip through Plate" >> prop { (input: List[TestData], seed: Int) =>
    val normalized = input map normalize
    val batchSize = max(1, abs(seed % 20))

    val plate = QDataPlate[IO, TestData, List[TestData]](isPrecise = true).unsafeRunSync()      // 💪
    val driver = QDataDriver(plate)

    val results =
      normalized
        .grouped(batchSize)
        .toList
        .foldMapM(driver.consume(_))

    val actual = for {
      rs <- results
      f <- driver.finish()
    } yield rs ::: f

    actual must beRight(normalized)
  }
}
