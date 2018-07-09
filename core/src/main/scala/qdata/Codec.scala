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

import java.time.{
  LocalDate,
  LocalDateTime,
  LocalTime,
  OffsetDateTime,
  OffsetTime,
}
import qdata.time.{DateTimeInterval, OffsetDate}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal
import scodec.{Attempt, Codec, DecodeResult, Err}
import scodec.bits.BitVector
import scodec.codecs.{
  ascii32,
  bool,
  discriminated,
  double,
  int64,
  optional,
  provide,
  uint4,
  utf8_32
}
import slamdata.Predef._
import spire.math.Real

class QDataCodec[A](qdata: QData[A]) {
  import QType._

  val longCodec: Version => Codec[A] = memoize { _ =>
    int64.xmap[A](qdata.makeLong, qdata.getLong)
  }

  val doubleCodec: Version => Codec[A] = memoize { _ =>
    double.xmap[A](qdata.makeDouble, qdata.getDouble)
  }

  val realCodec: Version => Codec[A] =
    exmapAscii[Real](
      Real.apply,
      qdata.makeReal,
      qdata.getReal(_).toString)

  val stringCodec: Version => Codec[A] = memoize { _ =>
    utf8_32.xmap[A](qdata.makeString, qdata.getString)
  }

  val nullCodec: Version => Codec[A] = memoize { _ =>
    provide(qdata.makeNull)
  }

  val booleanCodec: Version => Codec[A] = memoize { _ =>
    bool.xmap[A](qdata.makeBoolean, qdata.getBoolean)
  }

  val localDateTimeCodec: Version => Codec[A] =
    exmapAscii[LocalDateTime](
      LocalDateTime.parse,
      qdata.makeLocalDateTime,
      qdata.getLocalDateTime(_).toString)

  val localDateCodec: Version => Codec[A] =
    exmapAscii[LocalDate](
      LocalDate.parse,
      qdata.makeLocalDate,
      qdata.getLocalDate(_).toString)

  val localTimeCodec: Version => Codec[A] =
    exmapAscii[LocalTime](
      LocalTime.parse,
      qdata.makeLocalTime,
      qdata.getLocalTime(_).toString)

  val offsetDateTimeCodec: Version => Codec[A] =
    exmapAscii[OffsetDateTime](
      OffsetDateTime.parse,
      qdata.makeOffsetDateTime,
      qdata.getOffsetDateTime(_).toString)

  val offsetDateCodec: Version => Codec[A] =
    exmapAscii[OffsetDate](
      OffsetDate.parse,
      qdata.makeOffsetDate,
      qdata.getOffsetDate(_).toString)

  val offsetTimeCodec: Version => Codec[A] =
    exmapAscii[OffsetTime](
      OffsetTime.parse,
      qdata.makeOffsetTime,
      qdata.getOffsetTime(_).toString)

  val intervalCodec: Version => Codec[A] = memoize { _ =>
    ascii32.exmap[A](
      str =>
        Attempt.fromOption(
          DateTimeInterval.parse(str).map(qdata.makeInterval),
          Err(s"Failed to parse $str as a DateTimeInterval")),
      interval =>
        Attempt.successful(qdata.getInterval(interval).toString))
  }

  /* To encode an array, we first call `getArrayCursor` which
   * gives us a cursor into the array. We then enter a loop,
   * calling `hasNextArray(cursor)` to determine if there is
   * another element in the array. If there is another element,
   * we retrieve it using `getArrayAt(cursor)` and then loop
   * again, advancing the cursor with `stepArray(cursor)`. If
   * there is not another element, we return the `BitVector`
   * that we have been accumulating.
   *
   * To decode an array, we first call `prepArray` to obtain
   * an empty array onto which we'll push elements as we decode
   * them. We decode the first element, and call `pushArray`
   * to put the element in the accumulation array. We then loop
   * over the remainder, continuing until there are no more
   * elements, at which point we call `makeArray` on the
   * accumulator.
   */
  val arrayCodec: Version => Codec[A] = memoize { version =>
    // we encode with an extra bit prepended to each element of the encoded array
    // instead we could encode a single "stop" flag
    val codec: Codec[Option[A]] =
      optional(bool, Codec.lazily(qdataCodec(version)))

    def encoder(array: A): Attempt[BitVector] = {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      @tailrec
      def loop(cursor: qdata.ArrayCursor, prev: Attempt[BitVector]): Attempt[BitVector] =
        if (qdata.hasNextArray(cursor)) {
          val concat: Attempt[BitVector] =
            for {
              prevBits <- prev
              step <- codec.encode(Some(qdata.getArrayAt(cursor)))
            } yield prevBits ++ step

          loop(qdata.stepArray(cursor), concat)
        } else {
          for {
            prevBits <- prev
            end <- codec.encode(None)
          } yield prevBits ++ end
        }

      loop(qdata.getArrayCursor(array), Attempt.successful(BitVector.empty))
    }

    def decoder(bits: BitVector): Attempt[DecodeResult[A]] = {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      @tailrec
      def loop(prev: Attempt[DecodeResult[(Option[A], qdata.NascentArray)]])
          : Attempt[DecodeResult[A]] =
        prev match {
          // decoding remainder
          case Attempt.Successful(DecodeResult((Some(a), arr), remainder)) =>
            loop(
              codec.decode(remainder)
                .map(_.map(_ -> qdata.pushArray(a, arr))))
          // decoding completed successfully
          case Attempt.Successful(DecodeResult((None, arr), remainder)) =>
            Attempt.successful(DecodeResult(qdata.makeArray(arr), remainder))
          // decoding failed
          case failure @ Attempt.Failure(_) => failure
        }

      loop(
        codec.decode(bits)
          .map(_.map(_ -> qdata.prepArray)))
    }

    Codec[A](encoder(_), decoder(_))
  }

  /* To encode an object, we first call `getObjectCursor` which
   * gives us a cursor into the object. We then enter a loop,
   * calling `hasNextObject(cursor)` to determine if there is
   * another element pair in the object. If there is another pair,
   * we retrieve it using `getObjectAt(cursor)` and then loop
   * again, advancing the cursor with `stepObject(cursor)`. If
   * there is not another pair, we return the `BitVector`
   * that we have been accumulating.
   *
   * To decode an object, we first call `prepObject` to obtain
   * an empty object onto which we'll push element pairs as we decode
   * them. We decode the first pair, and call `pushObject`
   * to put the pair in the accumulation object. We then loop
   * over the remainder, continuing until there are no more
   * pairs, at which point we call `makeObject` on the
   * accumulator.
   */
  val objectCodec: Version => Codec[A] = memoize { version =>
    // we encode with an extra bit prepended to each element pair of the encoded object
    val codec: Codec[Option[(String, A)]] =
      optional(bool, Codec.lazily(utf8_32 ~ qdataCodec(version)))

    def encoder(obj: A): Attempt[BitVector] = {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      @tailrec
      def loop(cursor: qdata.ObjectCursor, prev: Attempt[BitVector]): Attempt[BitVector] =
        if (qdata.hasNextObject(cursor)) {
          val concat: Attempt[BitVector] =
            for {
              prevBits <- prev
              step <- codec.encode(
                Some(qdata.getObjectKeyAt(cursor) -> qdata.getObjectValueAt(cursor)))
            } yield prevBits ++ step

          loop(qdata.stepObject(cursor), concat)
        } else {
          for {
            prevBits <- prev
            end <- codec.encode(None)
          } yield prevBits ++ end
        }

      loop(qdata.getObjectCursor(obj), Attempt.successful(BitVector.empty))
    }

    def decoder(bits: BitVector): Attempt[DecodeResult[A]] = {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      @tailrec
      def loop(prev: Attempt[DecodeResult[(Option[(String, A)], qdata.NascentObject)]])
          : Attempt[DecodeResult[A]] =
        prev match {
          // decoding remainder
          case Attempt.Successful(DecodeResult((Some(key -> value), obj), remainder)) =>
            loop(
              codec.decode(remainder)
                .map(_.map(_ -> qdata.pushObject(key, value, obj))))
          // decoding completed successfully
          case Attempt.Successful(DecodeResult((None, obj), remainder)) =>
            Attempt.successful(DecodeResult(qdata.makeObject(obj), remainder))
          // decoding failed
          case failure @ Attempt.Failure(_) => failure
        }

      loop(
        codec.decode(bits)
          .map(_.map(_ -> qdata.prepObject)))
    }

    Codec[A](encoder(_), decoder(_))
  }

  val metaCodec: Version => Codec[A] = memoize { version =>
    val codec: Codec[(A, A)] =
      Codec.lazily(qdataCodec(version) ~ qdataCodec(version))

    codec.xmap[A](
      { case (value, meta) => qdata.makeMeta(value, meta) },
      a => (qdata.getMetaValue(a), qdata.getMetaMeta(a)))
  }

  def isType(qType: QType): PartialFunction[A, A] = {
    case a if qdata.tpe(a) eq qType => a
  }

  ////

  val qdataCodec: Version => Codec[A] = memoize { version =>
    discriminated[A].by(uint4)
      // primitive
      .subcaseP[A](0)(isType(QLong))(longCodec(version))
      .subcaseP[A](1)(isType(QDouble))(doubleCodec(version))
      .subcaseP[A](2)(isType(QReal))(realCodec(version))
      .subcaseP[A](3)(isType(QString))(stringCodec(version))
      .subcaseP[A](4)(isType(QBoolean))(booleanCodec(version))
      .subcaseP[A](5)(isType(QNull))(nullCodec(version))
      // datetime
      .subcaseP[A](6)(isType(QLocalDateTime))(localDateTimeCodec(version))
      .subcaseP[A](7)(isType(QLocalDate))(localDateCodec(version))
      .subcaseP[A](8)(isType(QLocalTime))(localTimeCodec(version))
      .subcaseP[A](9)(isType(QOffsetDateTime))(offsetDateTimeCodec(version))
      .subcaseP[A](10)(isType(QOffsetDate))(offsetDateCodec(version))
      .subcaseP[A](11)(isType(QOffsetTime))(offsetTimeCodec(version))
      .subcaseP[A](12)(isType(QInterval))(intervalCodec(version))
      // recursive
      .subcaseP[A](13)(isType(QArray))(arrayCodec(version))
      .subcaseP[A](14)(isType(QObject))(objectCodec(version))
      .subcaseP[A](15)(isType(QMeta))(metaCodec(version))
  }

  ////

  private def exmapAscii[B](parse: String => B, make: B => A, get: A => String)
      : Version => Codec[A] = memoize { _ =>
    ascii32.exmap[A](
      str => try {
        Attempt.successful(make(parse(str)))
      } catch {
        case NonFatal(ex) =>
          Attempt.failure(Err(s"Failed to parse $str\n${ex.getMessage}"))
      },
      value => Attempt.successful(get(value)))
  }

  private def memoize[A](f: Version => Codec[A]): Version => Codec[A] = {
    @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
    val memoized: mutable.Map[Version, Codec[A]] =
      mutable.Map[Version, Codec[A]]()

    { version =>
      memoized.get(version).getOrElse {
        val back: Codec[A] = f(version)
        memoized.update(version, back)
        back
      }
    }
  }
}
