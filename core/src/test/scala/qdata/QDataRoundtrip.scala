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

package qdata

import scalaz.\/
import slamdata.Predef._

final class QDataRoundtrip[A: QDataEncode: QDataDecode] {

  def roundtripUnsafe(data: A): A = QData.convert[A, A](data)

  def roundtrip(data: A): Option[A] =
    \/.fromTryCatchNonFatal(roundtripUnsafe(data)).toOption
}

object QDataRoundtrip {
  def apply[A: QDataEncode: QDataDecode]: QDataRoundtrip[A] =
    new QDataRoundtrip[A]
}
