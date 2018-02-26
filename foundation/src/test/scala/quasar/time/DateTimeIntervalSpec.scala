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

package quasar.time

import slamdata.Predef._

import java.time._

class DateTimeIntervalSpec extends quasar.Qspec {

  "parsing" should {
    "not parse just a P" in {
      DateTimeInterval.parse("P") shouldEqual None
    }
    "not parse just a fraction" in {
      DateTimeInterval.parse("PT.005S") shouldEqual None
    }
    "parse just a year" in {
      DateTimeInterval.parse("P1Y") shouldEqual Some(DateTimeInterval(1, 0, 0, 0, 0))
    }
    "parse just a month" in {
      DateTimeInterval.parse("P1M") shouldEqual Some(DateTimeInterval(0, 1, 0, 0, 0))
    }
    "parse just a week" in {
      DateTimeInterval.parse("P1W") shouldEqual Some(DateTimeInterval(0, 0, 7, 0, 0))
    }
    "parse just a day" in {
      DateTimeInterval.parse("P1D") shouldEqual Some(DateTimeInterval(0, 0, 1, 0, 0))
    }
    "parse just an hour" in {
      DateTimeInterval.parse("PT1H") shouldEqual Some(DateTimeInterval(0, 0, 0, 60L * 60, 0))
    }
    "parse just a minute" in {
      DateTimeInterval.parse("PT1M") shouldEqual Some(DateTimeInterval(0, 0, 0, 60L, 0))
    }
    "parse just a second" in {
      DateTimeInterval.parse("PT1S") shouldEqual Some(DateTimeInterval(0, 0, 0, 1, 0))
    }
    "parse a composite duration with every component" in {
      DateTimeInterval.parse("P1Y2M3W4DT5H6M7.8S") shouldEqual Some(DateTimeInterval(1, 2, 25, 18367L, 800000000))
    }
    "parse a negated composite duration with every component" in {
      DateTimeInterval.parse("-P1Y2M3W4DT5H6M7.8S") shouldEqual Some(DateTimeInterval(-1, -2, -25, -18367L, -800000000))
    }
    "parse a composite duration with every component negated" in {
      DateTimeInterval.parse("P-1Y-2M-3W-4DT-5H-6M-7.8S") shouldEqual Some(DateTimeInterval(-1, -2, -25, -18367L, -800000000))
    }
    "parse should handle negative nanos with seconds == 0" in {
      val str = "P2131840384M91413566DT-488689H-42M-0.917127768S"
      DateTimeInterval.parse(str).map(_.toString) shouldEqual Some(str)
    }
  }

  "toString" should {
    "empty" in {
      DateTimeInterval(0, 0, 0, 0L, 0).toString shouldEqual "P0D"
    }
    "one year" in {
      DateTimeInterval(1, 0, 0, 0L, 0).toString shouldEqual "P1Y"
    }
    "one month" in {
      DateTimeInterval(0, 1, 0, 0L, 0).toString shouldEqual "P1M"
    }
    "one day" in {
      DateTimeInterval(0, 0, 1, 0L, 0).toString shouldEqual "P1D"
    }
    "one hour" in {
      DateTimeInterval(0, 0, 0, 3600L, 0).toString shouldEqual "PT1H"
    }
    "one minute" in {
      DateTimeInterval(0, 0, 0, 60L, 0).toString shouldEqual "PT1M"
    }
    "one second" in {
      DateTimeInterval(0, 0, 0, 1L, 0).toString shouldEqual "PT1S"
    }
    "one tenth second" in {
      DateTimeInterval(0, 0, 0, 0L, 100000000).toString shouldEqual "PT0.1S"
    }
    "all components" in {
      DateTimeInterval(1, 1, 1, 3661L, 100000000).toString shouldEqual "P1Y1M1DT1H1M1.1S"
    }
    "negative nanos" in {
      Duration.ofSeconds(1, -100000000).toString shouldEqual "PT0.9S"
      DateTimeInterval(0, 0, 0, 1L, -100000000).toString shouldEqual "PT0.9S"
    }
  }

  "factories" should {
    "one year" in {
      DateTimeInterval.ofYears(1) shouldEqual DateTimeInterval(1, 0, 0, 0, 0)
    }
    "one month" in {
      DateTimeInterval.ofMonths(1) shouldEqual DateTimeInterval(0, 1, 0, 0, 0)
    }
    "one day" in {
      DateTimeInterval.ofDays(1) shouldEqual DateTimeInterval(0, 0, 1, 0, 0)
    }
    "one hour" in {
      DateTimeInterval.ofHours(1L) shouldEqual DateTimeInterval(0, 0, 0, 3600, 0)
    }
    "one minute" in {
      DateTimeInterval.ofMinutes(1L) shouldEqual DateTimeInterval(0, 0, 0, 60, 0)
    }
    "one second" in {
      DateTimeInterval.ofSeconds(1L) shouldEqual DateTimeInterval(0, 0, 0, 1, 0)
    }
    "one second and a half" in {
      DateTimeInterval.ofSecondsNanos(1L, 500000000L) shouldEqual DateTimeInterval(0, 0, 0, 1L, 500000000L)
    }
    "one half-second" in {
      DateTimeInterval.ofNanos(500000000L) shouldEqual DateTimeInterval(0, 0, 0, 0L, 500000000L)
    }
    "one millis" in {
      DateTimeInterval.ofMillis(1L) shouldEqual DateTimeInterval(0, 0, 0, 0L, 1000000L)
    }
    "one nano" in {
      DateTimeInterval.ofNanos(1L) shouldEqual DateTimeInterval(0, 0, 0, 0L, 1L)
    }
    "negative nanos" in {
      DateTimeInterval.ofNanos(-1L) shouldEqual DateTimeInterval(0, 0, 0, -1L, 999999999L)
    }
  }

  "compareTo" should {
    "compare seconds downward" in {
      DateTimeInterval.ofMillis(1).compareTo(DateTimeInterval.ofHours(1)) should be_<(0)
    }
    "compare seconds upward" in {
      DateTimeInterval.ofHours(1).compareTo(DateTimeInterval.ofMillis(1)) should be_>(0)
    }
    "compare all components" in {
      DateTimeInterval(0, 0, 0, 0, 0).compareTo(DateTimeInterval(1, 1, 1, 1, 1)) should be_<(0)
    }
    "compare nanos" in {
      DateTimeInterval.ofSecondsNanos(1, 5).compareTo(DateTimeInterval.ofSeconds(1)) should be_<(0)
    }
  }
}
