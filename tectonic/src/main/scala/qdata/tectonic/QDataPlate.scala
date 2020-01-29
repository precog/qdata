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

import slamdata.Predef._

import cats.effect.Sync

import qdata.QDataEncode
import qdata.json.{NumericParser, PreciseParser}

import java.lang.CharSequence

import scala.collection.compat._
import scala.collection.mutable.Builder
import scala.sys.error

import tectonic.{Plate, Signal}

@SuppressWarnings(Array(
  "org.wartremover.warts.ToString",
  "org.wartremover.warts.TraversableOps",
  "org.wartremover.warts.Var"))
final class QDataPlate[A, R] private (
    isPrecise: Boolean)(
    implicit A: QDataEncode[A], cbf: Factory[A, R])
    extends Plate[R] {

  private final class ArrCtx() {
    private var arr: A.NascentArray = A.prepArray

    def add(a: A): Unit = {
      arr = A.pushArray(a, arr)
    }

    def finish: A =
      A.makeArray(arr)
  }

  private final class ObjCtx() {
    private var obj: A.NascentObject = A.prepObject

    def add(k: String, v: A): Unit = {
      obj = A.pushObject(k, v, obj)
    }

    def finish: A =
      A.makeObject(obj)
  }

  private sealed trait CNode {
    def mapNonMeta(f: CNode => CNode): CNode =
      this match {
        case Meta(n, m) => Meta(f(n), m)
        case other => f(other)
      }

    def unMeta: (CNode, Option[ObjCtx]) =
      this match {
        case Meta(n, m) => (n, Some(m))
        case other => (other, None)
      }
  }

  // NB: 'sealed' instead of 'final' to avoid warnings during patmat
  //     due to https://issues.scala-lang.org/browse/SI-4440
  private sealed case class Value(v: A) extends CNode

  private sealed case class MapKey(k: String) extends CNode
  private sealed case class MapAssoc(k: String, v: A) extends CNode
  private sealed case class MapAssocS(k: String, s: String) extends CNode
  private sealed case class Map(obj: ObjCtx) extends CNode

  private sealed case class ArrNew() extends CNode
  private sealed case class ArrElt(v: A) extends CNode
  private sealed case class Arr(arr: ArrCtx) extends CNode

  private sealed case class MetaKey(mk: String) extends CNode
  private sealed case class MetaAssoc(mk: String, mv: A) extends CNode
  private sealed case class UnboundMeta(obj: ObjCtx) extends CNode
  private sealed case class Meta(v: CNode, obj: ObjCtx) extends CNode

  private val numericParser: NumericParser[A] = NumericParser[A]
  private val preciseParser: PreciseParser[A] = PreciseParser[A]
  private var builder: Builder[A, R] = cbf.newBuilder
  private var cursor: List[CNode] = Nil

  def nul(): Signal = {
    absorbValue(A.makeNull)
    Signal.Continue
  }

  def fls(): Signal = {
    absorbValue(A.makeBoolean(false))
    Signal.Continue
  }

  def tru(): Signal = {
    absorbValue(A.makeBoolean(true))
    Signal.Continue
  }

  def map(): Signal = {
    absorbValue(A.makeObject(A.prepObject))
    Signal.Continue
  }

  def arr(): Signal = {
    absorbValue(A.makeArray(A.prepArray))
    Signal.Continue
  }

  def num(s: CharSequence, decIdx: Int, expIdx: Int): Signal = {
    absorbValue(numericParser.parseNumber(s.toString, decIdx, expIdx))
    Signal.Continue
  }

  def str(s: CharSequence): Signal = {
    absorbString(s.toString)
    Signal.Continue
  }

  def nestMap(pathComponent: CharSequence): Signal = {
    cursor ::= MapKey(pathComponent.toString)
    Signal.Continue
  }

  def nestArr(): Signal = {
    cursor ::= ArrNew()
    Signal.Continue
  }

  def nestMeta(pathComponent: CharSequence): Signal = {
    cursor ::= MetaKey(pathComponent.toString)
    Signal.Continue
  }

  @tailrec
  def unnest(): Signal = {
    val head = cursor.head
    cursor = cursor.tail

    val (node, meta) = head.unMeta

    node match {
      case MapAssoc(k, v) =>
        unnestMap(k, v, meta)
        Signal.Continue

      case MapAssocS(k, s) if (isPrecise && preciseParser.isPreciseKey(k)) =>
        absorbValue(buildValue(preciseParser.parsePrecise(k, s), meta))
        Signal.Continue

      case MapAssocS(k, s) =>
        unnestMap(k, A.makeString(s), meta)
        Signal.Continue

      case Map(ctx) =>
        absorbValue(buildValue(ctx.finish, meta))
        unnest()

      case ArrElt(v) =>
        cursor match {
          case Arr(ctx) :: _ =>
            ctx.add(buildValue(v, meta))
            Signal.Continue

          case Meta(Arr(ctx), _) :: _ =>
            ctx.add(buildValue(v, meta))
            Signal.Continue

          case _ =>
            val ctx = new ArrCtx
            ctx.add(buildValue(v, meta))
            cursor ::= Arr(ctx)
            Signal.Continue
        }

      case Arr(ctx) =>
        absorbValue(buildValue(ctx.finish, meta))
        unnest()

      case MetaAssoc(k, v) =>
        cursor match {
          case Meta(_, ctx) :: _ =>
            ctx.add(k, buildValue(v, meta))
            Signal.Continue

          case UnboundMeta(ctx) :: _ =>
            ctx.add(k, buildValue(v, meta))
            Signal.Continue

          case h :: t =>
            val ctx = new ObjCtx
            ctx.add(k, buildValue(v, meta))
            cursor = Meta(h, ctx) :: t
            Signal.Continue

          case Nil =>
            val ctx = new ObjCtx
            ctx.add(k, buildValue(v, meta))
            cursor = UnboundMeta(ctx) :: Nil
            Signal.Continue
        }

      case other => error(s"unnest(): Unexpected $other")
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def finishRow(): Unit =
    if (cursor.nonEmpty) {
      val h = cursor.head
      val t = cursor.tail

      if (t.isEmpty) {
        val (node, meta) = h.unMeta

        node match {
          case Map(ctx) =>
            builder += buildValue(ctx.finish, meta)
            cursor = Nil

          case Arr(ctx) =>
            builder += buildValue(ctx.finish, meta)
            cursor = Nil

          case Value(v) =>
            builder += buildValue(v, meta)
            cursor = Nil

          case other =>
            error(s"finishRow(): Unexpected $other")
        }
      } else {
        error(s"finishRow(): Unexpected $cursor")
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def finishBatch(terminal: Boolean): R = {
    val r = builder.result

    builder = cbf.newBuilder

    r
  }

  def skipped(bytes: Int): Unit = ()

  ////

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def absorbString(s: String): Unit =
    if (cursor.isEmpty) {
      cursor = Value(A.makeString(s)) :: Nil
    } else {
      val newHead = cursor.head mapNonMeta {
        case UnboundMeta(ctx) => Meta(Value(A.makeString(s)), ctx)
        case MapKey(k) => MapAssocS(k, s)
        case ArrNew() => ArrElt(A.makeString(s))
        case MetaKey(k) => MetaAssoc(k, A.makeString(s))
        case other => error(s"absorbString(): Unexpected $other")
      }

      cursor = newHead :: cursor.tail
    }

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def absorbValue(v: A): Unit =
    if (cursor.isEmpty) {
      cursor = Value(v) :: Nil
    } else {
      val newHead = cursor.head mapNonMeta {
        case UnboundMeta(ctx) => Meta(Value(v), ctx)
        case MapKey(k) => MapAssoc(k, v)
        case ArrNew() => ArrElt(v)
        case MetaKey(k) => MetaAssoc(k, v)
        case other => error(s"absorbValue(): Unexpected $other")
      }

      cursor = newHead :: cursor.tail
    }

  private def buildValue(data: A, meta: Option[ObjCtx]): A =
    meta.fold(data)(m => A.makeMeta(data, m.finish))

  private def unnestMap(k: String, v: A, meta: Option[ObjCtx]): Unit =
    cursor match {
      case Map(ctx) :: _ =>
        ctx.add(k, buildValue(v, meta))

      case Meta(Map(ctx), _) :: _ =>
        ctx.add(k, buildValue(v, meta))

      case _ =>
        val ctx = new ObjCtx
        ctx.add(k, buildValue(v, meta))
        cursor ::= Map(ctx)
    }
}

object QDataPlate {
  def apply[F[_]: Sync, A: QDataEncode, R](isPrecise: Boolean)(implicit cbf: Factory[A, R]): F[Plate[R]] =
    Sync[F].delay(new QDataPlate[A, R](isPrecise))
}
