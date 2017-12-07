package skiticket.utils

import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import javax.crypto.Mac

import scodec.bits.BitVector
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import skiticket.utils.ByteSeqExtension._

package object codecs {
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

    def localDateDaysCodec[T](baseDate: LocalDate, numCodec: Codec[T])
                             (implicit integral: Integral[T]): Codec[LocalDate] =
        numCodec.exmap({
            num =>
                Attempt.successful(baseDate.plusDays(integral.toLong(num)))
        }, { date =>
            val days = ChronoUnit.DAYS.between(baseDate, date)
            Attempt.successful(integral.fromInt(days.toInt))
        })

    def localDateTimeCodec: Codec[LocalDateTime] = uint32L.exmap[LocalDateTime](
        x => Attempt.Successful(LocalDateTime.ofEpochSecond(x, 0, ZoneOffset.UTC)),
        x => Attempt.successful(x.toEpochSecond(ZoneOffset.UTC))
    )

    def macCodec[T](macInstance: Mac, suffix: BitVector)(implicit codec: Codec[T]) =
        filtered(codec, new
                        Codec[BitVector] {
            override def encode(value: BitVector): Attempt[BitVector] = {
                val bytes = value.toByteArray

                macInstance.update(bytes)
                macInstance.update(suffix.toByteArray)
                val mac = macInstance.doFinal()
                Attempt.successful(value ++ BitVector(mac))
            }

            private val macLength = macInstance.getMacLength

            override def sizeBound: SizeBound =
                codec.sizeBound + SizeBound.exact(macLength)

            override def decode(bits: BitVector)
            : Attempt[DecodeResult[BitVector]] = {
                val bytes = bits.bytes
                val (dataBytes, macBytes) = bytes.splitAt(bytes.size - macLength)
                assert(macBytes.length == macLength)

                macInstance.update(dataBytes.toArray)
                macInstance.update(suffix.toByteArray)
                val macComputed = macInstance.doFinal()

                if (macComputed sameElements macBytes.toArray) {
                    Attempt.successful(
                        DecodeResult(
                            dataBytes.toBitVector,
                            BitVector.empty
                        )
                    )
                } else {
                    Attempt.failure(Err(s"MAC did not match (expected ${
                        macBytes
                                .toHex
                    }, got ${macComputed.toSeq.toHexString}"))
                }
            }
        })
}

