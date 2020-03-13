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
import qdata.QDataEncode

import org.typelevel.jawn.{Facade, FContext}

import java.lang.CharSequence

@SuppressWarnings(Array(
  "org.wartremover.warts.Equals",
  "org.wartremover.warts.Null",
  "org.wartremover.warts.ToString",
  "org.wartremover.warts.Var"))
final class QDataFacade[J] private (isPrecise: Boolean)(implicit qd: QDataEncode[J])
    extends Facade.NoIndexFacade[J] {

  private val numericParser: NumericParser[J] = NumericParser[J]

  def jnull: J = qd.makeNull

  def jfalse: J = qd.makeBoolean(false)
  def jtrue: J = qd.makeBoolean(true)

  def jnum(s: CharSequence, decIndex: Int, expIndex: Int): J =
    numericParser.parseNumber(s.toString, decIndex, expIndex)

  def jstring(s: CharSequence): J = qd.makeString(s.toString)

  def singleContext(): FContext[J] =
    new FContext.NoIndexFContext[J] {
      var result: J = _

      def add(s: CharSequence): Unit = { result = jstring(s) }
      def add(v: J): Unit = { result = v }
      def finish: J = result
      def isObj: Boolean = false
    }

  def arrayContext(): FContext[J] =
    new ArrayContext[J]

  def objectContext(): FContext[J] =
    if (isPrecise)
      new PreciseObjectContext[J]
    else
      new ObjectContext[J]
}

object QDataFacade {
  def apply[J: QDataEncode](isPrecise: Boolean): Facade[J] =
    new QDataFacade(isPrecise)
}
