package skiticket

import java.security.MessageDigest
import java.time._

import scodec.bits._
import skiticket.nfc.{NfcConstants, NfcTools}
import skiticket.utils.ByteSeqExtension._

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
        nfc.memory(NfcTicket.TagPage) == NfcTicket.TagBytes
    }

    def readData(): Ticket = {
        readData(nfc.getCounter)
    }

    private def readData(currentCounter: Int): Ticket = {
        val codec = Ticket.codec(authenticationKey, currentCounter)
        val sizeInPages = (codec.sizeBound.lowerBound.toInt / 8 + NfcConstants .PageSize - 1) / NfcConstants.PageSize
        val startPage = NfcTicket.DataPage + sizeInPages * currentCounter % 2

        val bytes = nfc.memory(startPage, sizeInPages)
        val data = codec.decode(BitVector.view(bytes.toArray)).require.value

        data
    }

    def writeData(data: Ticket): Unit = {
        writeData(data, nfc.getCounter)
    }

    private def writeData(data: Ticket, currentCounter: Int): Unit = {
        val nextCounter = currentCounter + 1

        val codec = Ticket.codec(authenticationKey, nextCounter)

        val bytes = codec.encode(data).require.toByteArray

        val sizeInPages = (codec.sizeBound.lowerBound.toInt / 8 + NfcConstants .PageSize - 1) / NfcConstants.PageSize
        assert(codec.sizeBound.lowerBound.toInt / 8 == bytes.length)
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
            Ticket(
                0,
                List[Ticket.Subscription](None, None, None),
                LocalDateTime.ofEpochSecond(0, 0, ZoneOffset.UTC)
            )
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

