package skiticket.utils

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import javax.crypto.Mac

import scodec.Attempt.{Failure, Successful}
import scodec.bits.{BitVector, ByteVector}
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import skiticket.utils.ByteSeqExtension._

package object codecs {
    /**
      * Codec for option that gives Some if value is non null.
      * @param codec codec to wrap
      * @param zeroVal zero value for codec
      * @tparam T type of codec
      * @return wrapped codec, Some if value present, None otherwise
      */
    def presentIfNonZero[T](codec: Codec[T], zeroVal: T): Codec[Option[T]] = new
                    Codec[Option[T]] {
        override def decode(bits: BitVector): Attempt[DecodeResult[Option[T]]] = {
            codec.map { x =>
                if (x == zeroVal) {
                    None
                } else {
                    Some(x)
                }
            }.decode(bits)
        }

        override def encode(value: Option[T]): Attempt[BitVector] = {
            value match {
                case Some(x) =>
                    codec.encode(x)
                case None =>
                    codec.encode(zeroVal)
            }
        }

        override def sizeBound: SizeBound = codec.sizeBound
    }

    /**
      * Writes a datetime as an offset number of seconds
      * @param baseDate epoch
      * @param numCodec codec to store number of seconds
      * @param integral numeric type manipulation
      * @tparam T numeric type
      * @return codec
      */
    def offsetDateTimeCodec[T](baseDate: LocalDateTime, numCodec: Codec[T])
                              (implicit integral: Integral[T])
    : Codec[LocalDateTime] =
        numCodec.exmap(
            num =>
                Attempt.successful(baseDate.plusSeconds(integral.toLong(num))),
            { dateTime =>
                if (dateTime.isBefore(baseDate)) {
                    throw new IllegalArgumentException("Need datetime before " +
                            "base date")
                }

                require(dateTime.getNano == 0)
                require(baseDate.getNano == 0)

                val seconds = ChronoUnit.SECONDS.between(baseDate, dateTime)
                Attempt.successful(integral.fromInt(seconds.toInt))
            })

    /**
      * Codec for MAC appending
      * @param macInstance MAC instance for calculating MAC, requires keyed
      * @param suffix eventual suffix to append to codec before mac calculation
      * @param valueCodec
      * @tparam T
      * @return
      */
    def macCodec[T](macInstance: Mac, suffix: BitVector)(implicit valueCodec: Codec[T]): Codec[T] =
        new Codec[T] {
            require(valueCodec.sizeBound.exact.isDefined)

            private val macLength = macInstance.getMacLength

            override def encode(value: T): Attempt[BitVector] = {
                valueCodec.encode(value) match {
                    case Successful(valueBits) =>
                        macInstance.update(valueBits.toByteArray)
                        macInstance.update(suffix.toByteArray)
                        val mac = BitVector.view(macInstance.doFinal())

                        assert(mac.size / 8 == macLength,
                            s"Is ${mac.size / 8}, should be $macLength")

                        Attempt.successful(valueBits ++ mac)
                    case attempt => attempt
                }
            }

            override def sizeBound: SizeBound =
                valueCodec.sizeBound + SizeBound.exact(macLength * 8)

            override def decode(allBits: BitVector): Attempt[DecodeResult[T]] = {
                valueCodec.decode(allBits) match {
                    case Successful(DecodeResult(_, BitVector.empty)) =>
                        Attempt.failure(Err("Missing MAC bits"))
                    case Successful(DecodeResult(value, valueRemainder)) =>
                        val (macExpected, remainder) = valueRemainder.bytes.splitAt(macLength)

                        val valueBits = allBits.dropRight(valueRemainder.length)
                        macInstance.update(valueBits.toByteArray)
                        macInstance.update(suffix.toByteArray)
                        val macComputed = ByteVector.view(macInstance.doFinal())

                        if (macComputed == macExpected) {
                            Attempt.successful(DecodeResult(value, remainder.bits))
                        } else {
                            Attempt.failure(Err(s"MAC did not match (expected" +
                                    s" ${macExpected.toHex}, got " +
                                    s"${macComputed.toHex}"))
                        }
                    case e@Failure(_) => e
                }
            }
        }

    /**
      * A exact-size sequence of fixed size elements, with proper sizeBound-ing
      * @param size size of sequence
      * @param codec element exact-size codec
      * @tparam T type of element
      * @return fixed size sequence codec
      */
    def knownSizeSeq[T](size: Int, codec: Codec[T]): Codec[Seq[T]] = new
                    Codec[Seq[T]] {
        require(codec.sizeBound.exact.isDefined)

        override def encode(value: Seq[T]): Attempt[BitVector] = {
            require(value.lengthCompare(size) == 0)
            val attempt = value.map(codec.encode)
                    .fold(Attempt.successful(BitVector.empty)) {
                        case (Successful(bits1), Successful(bits2)) =>
                            Attempt.successful(bits1 ++ bits2)
                        case (Failure(err1), Failure(err2)) =>
                            Attempt.failure(Err(s"$err1\n$err2"))
                        case (Failure(err), _) =>
                            Attempt.failure(err)
                        case (_, Failure(err)) =>
                            Attempt.failure(err)
                    }

            attempt
        }

        override def sizeBound: SizeBound = codec.sizeBound * size

        override def decode(bits: BitVector): Attempt[DecodeResult[Seq[T]]] = {
            val elementSize = codec.sizeBound.exact.get
            val attempts = bits
                    .take(elementSize * size)
                    .grouped(elementSize)
                    .zipWithIndex
                    .map { case (elemBits, i) => (codec.decode(elemBits), i) }

            val errors = attempts.collect {
                case (Failure(err), i) =>
                    s"$i -> $err"
            }

            if (errors.isEmpty) {
                Attempt.successful(DecodeResult(
                    attempts.map(x => x._1.require.value),
                    bits.drop(elementSize * size)
                ))
            } else {
                Attempt.failure(Err(errors.mkString("\n")))
            }
        }
    }
}

