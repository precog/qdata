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

package qdata

import slamdata.Predef.Vector

import org.specs2.ScalaCheck
import org.specs2.mutable.SpecLike
import scodec.{Attempt, DecodeResult, Codec}
import scodec.bits.BitVector
import spire.math.Real

object QDataCodecSpec extends SpecLike with ScalaCheck {
  import TestDataGenerators._

  "memoize all the codecs" in {
    val qdata = QDataCodec[TestData]
    val version = Version.Latest

    (qdata.longCodec(version) eq qdata.longCodec(version)) must beTrue
    (qdata.doubleCodec(version) eq qdata.doubleCodec(version)) must beTrue
    (qdata.realCodec(version) eq qdata.realCodec(version)) must beTrue
    (qdata.stringCodec(version) eq qdata.stringCodec(version)) must beTrue
    (qdata.nullCodec(version) eq qdata.nullCodec(version)) must beTrue
    (qdata.booleanCodec(version) eq qdata.booleanCodec(version)) must beTrue

    (qdata.localDateTimeCodec(version) eq qdata.localDateTimeCodec(version)) must beTrue
    (qdata.localDateCodec(version) eq qdata.localDateCodec(version)) must beTrue
    (qdata.localTimeCodec(version) eq qdata.localTimeCodec(version)) must beTrue
    (qdata.offsetDateTimeCodec(version) eq qdata.offsetDateTimeCodec(version)) must beTrue
    (qdata.offsetDateCodec(version) eq qdata.offsetDateCodec(version)) must beTrue
    (qdata.offsetTimeCodec(version) eq qdata.offsetTimeCodec(version)) must beTrue
    (qdata.intervalCodec(version) eq qdata.intervalCodec(version)) must beTrue

    (qdata.arrayCodec(version) eq qdata.arrayCodec(version)) must beTrue
    (qdata.objectCodec(version) eq qdata.objectCodec(version)) must beTrue
    (qdata.metaCodec(version) eq qdata.metaCodec(version)) must beTrue

    (qdata.qdataCodec(version) eq qdata.qdataCodec(version)) must beTrue
  }

  "roundtrip nested array" >> {
    val codec: Codec[TestData] =
      QDataCodec[TestData].qdataCodec(Version.Latest)

    val dataArray: TestData =
      TestData._Array(Vector(
        TestData._Array(Vector(
          TestData._Real(Real("123")),
          TestData._Long(5L),
          TestData._Long(6L),
          TestData._Long(6L),
          TestData._Long(6L),
          TestData._Long(6L),
          TestData._Long(6L),
          TestData._Boolean(false),
          TestData._Long(9L),
          TestData._Double(7.0)))))

    roundtrip[TestData](codec, dataArray)
  }

  "roundtrip nested object" >> {
    val codec: Codec[TestData] =
      QDataCodec[TestData].qdataCodec(Version.Latest)

    val dataObject: TestData =
      TestData._Object(Vector(
        ("outerKey1", TestData._Object(Vector(
          ("innerKey1", TestData._Boolean(true)),
          ("innerKey2", TestData._Boolean(false)),
          ("innerKey3", TestData._Null()))))))

    roundtrip[TestData](codec, dataObject)
  }

  "roundtrip unicode string" >> {
    val codec: Codec[TestData] =
      QDataCodec[TestData].qdataCodec(Version.Latest)

    val data: TestData = TestData._String("넼매뉫썾 foobarbaz")

    roundtrip[TestData](codec, data)
  }

  "roundtrip unicode-keyed object" >> {
    val codec: Codec[TestData] =
      QDataCodec[TestData].qdataCodec(Version.Latest)

    val data: TestData = TestData._Object(Vector(
      ("넼매뉫썾 foobarbaz", TestData._Null())))

    roundtrip[TestData](codec, data)
  }

  "roundtrip arbitrary test data" >> {
    val codec: Codec[TestData] =
      QDataCodec[TestData].qdataCodec(Version.Latest)

    prop { data: TestData =>
      roundtrip[TestData](codec, data)
    }.set(minTestsOk = 500)
  }

  // cribbed from https://github.com/scodec/scodec/blob/series/1.10.x/testkit/shared/src/main/scala/scodec/CodecSuite.scala
  def roundtrip[A](codec: Codec[A], value: A) = {
    val encoded = codec.encode(value)
    encoded.isSuccessful must_=== true
    val Attempt.Successful(DecodeResult(decoded, remainder)) = codec.decode(encoded.require)
    remainder must_=== BitVector.empty
    decoded must_=== value
  }
}
