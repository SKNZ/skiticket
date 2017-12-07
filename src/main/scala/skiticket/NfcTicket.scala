package skiticket

import java.security.MessageDigest
import java.time._
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scodec.Codec
import scodec.bits._
import scodec.codecs._
import skiticket.TicketData.Subscriptions
import skiticket.nfc.{NfcConstants, NfcTools}
import skiticket.utils.ByteSeqExtension._
import skiticket.utils.codecs._

case class TicketData(ridesRemaining: Int,
                      subscriptions: Subscriptions,
                      lastValidation: LocalDateTime) {
    require(ridesRemaining >= 0)
    require(ridesRemaining <= TicketData.MaxNumberOfRides)

    {
        val maxDate: LocalDate =
            LocalDate.now().plusDays(TicketData.MaxSubscriptionLength)

        subscriptions.foreach {
            case Some(Left(expiryDate: LocalDate)) =>
                require(maxDate.isAfter(expiryDate))

            case Some(Right(days: Int)) =>
                require(days > 0
                        && days <= TicketData.MaxSubscriptionLength)

            case None =>
        }
    }

    {
        val timeSinceLastValidation =
            Duration.between(lastValidation, LocalDateTime.now())

        require(timeSinceLastValidation.compareTo(TicketData.AntiPassBackTimer) >= 0)
    }
}

object TicketData {
    val NumberOfSubscriptions = 3
    type Subscription = Option[Either[LocalDate, Int]]
    type Subscriptions = List[Subscription]

    val MaxNumberOfRides = 100
    val MaxSubscriptionLength = 365
    val AntiPassBackTimer: Duration = Duration.ofMinutes(1)

    val BaseDate: LocalDate = LocalDate.of(2017, 12, 3)

    def codec(keyBytes: Seq[Byte], counter: Int): Codec[TicketData] = {
        val daysCodec = uintL(15)

        val macAlgorithm = "HmacMD5"
        val key = new SecretKeySpec(keyBytes.toArray, macAlgorithm)
        val macInstance = Mac.getInstance(macAlgorithm)
        macInstance.init(key)

        macCodec(macInstance, uint32L.encode(counter).require) {
            ("ridesRemaining" | uint16L) ::
                    ("subscriptions" | listOfN(
                        provide(NumberOfSubscriptions),
                        presentIfNonZero(
                            either(
                                bool(1),
                                localDateDaysCodec(BaseDate, daysCodec),
                                daysCodec
                            ),
                            Right(0)
                        )
                    )) ::
                    ("lastValidation" | localDateTimeCodec)
        }.as[TicketData]
    }
}

case class NfcTicket(nfc: NfcTools) {
    val Uid: Seq[Byte] = {
        val key = nfc.memory(0, 2)
        println(s"UID: ${key.toHexString}")
        key
    }

    private lazy val authenticationKey: Seq[Byte] = {
        val sha256 = MessageDigest.getInstance("SHA-256")
        val uidDigest = sha256.digest(Uid.toArray)
        val masterKeyDigest = sha256.digest(NfcTicket.MasterKey.getBytes())

        sha256.update(uidDigest)
        val key = sha256.digest(masterKeyDigest).toSeq

        key
    }

    private lazy val desAuthenticationKey: Seq[Byte] =
        authenticationKey.slice(0, NfcConstants.KeySize)

    def isSkiTicket: Boolean = {
        nfc.memory(NfcTicket.TagPage) sameElements NfcTicket.TagBytes
    }

    private val macAlgorithm: String = "HmacMD5"

    def readData(currentCounter: Int): TicketData = {
        val codec = TicketData.codec(authenticationKey, currentCounter)
        val sizeInPages = (codec.sizeBound.exact.get.toInt / 8+ NfcConstants.PageSize - 1) / NfcConstants.PageSize
        val startPage = NfcTicket.DataPage + sizeInPages * currentCounter % 2

        val bytes = nfc.memory(startPage, sizeInPages)
        val data = codec.decode(BitVector.view(writtenBytes.toArray)).require.value

        data
    }

    def writeData(data: TicketData, currentCounter: Int): Unit = {
        val nextCounter = currentCounter + 1

        val codec = TicketData.codec(authenticationKey, nextCounter)

        val bytes = codec.encode(data).require.toByteArray

        val sizeInPages = (codec.sizeBound.exact.get.toInt / 8 + NfcConstants.PageSize - 1) / NfcConstants.PageSize
        assert(codec.sizeBound.exact.get / 8 == bytes.length)
        val startPage = NfcTicket.DataPage + sizeInPages * nextCounter % 2

        nfc.memory(startPage, sizeInPages).update(bytes.toSeq)
        nfc.save()

        val writtenData = readData(nextCounter)
        assert(data == writtenData)

        nfc.incrementCounter()
    }

    def format(): Unit = {
        println("Card needs formatting")
        assert(nfc.utils.eraseMemory())

        assert(nfc.utils.setAuth0(NfcTicket.TagPage))
        assert(nfc.utils.setAuth1(true))

        println("Writing application tag.")
        nfc.memory(NfcTicket.TagPage, 1).update(NfcTicket.TagBytes)

        nfc.save()

        assert(nfc.utils.changeKey(desAuthenticationKey.toArray))
        println("3DES key changed.")

        writeData(
            TicketData(
                0,
                List[TicketData.Subscription](None, None, None),
                LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)
            ),
            nfc.getCounter
        )
    }

    def authenticate(): Unit = {
        if (!nfc.authenticate(NfcConstants.DefaultKey)) {
            println("3DES authentication with default key failed.")

            assert(nfc.authenticate(desAuthenticationKey),
                "3DES authentication with UID key failed.")
        }

        println("3DES authenticated.")
    }
}

object NfcTicket {
    val TagPage = 4

    val DataPage = 5

    val CounterPage = 41

    val MasterKey = "9gWwas51JqcG1zoRBgErqRRLdvIOkxS3H5WNO3wM6uRj982unYOxxm2eh3Vwvs1aOZM5kS2yKOAJKwPvw4zYjBu0krh5fMTLMdPq"

    val TagBytes: Seq[Byte] = "SKI0".getBytes()
}

