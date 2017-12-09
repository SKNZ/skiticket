package skiticket

import java.security.MessageDigest

import scodec.bits._
import scodec.codecs._
import skiticket.nfc.{NfcConstants, NfcException, NfcTools}
import skiticket.utils.ByteSeqExtension._

case class NfcTicket(nfc: NfcTools) {
    val UidBytes: Seq[Byte] = {
        val uidBytes = nfc.memory(0, 2)
        println(s"UIDB: ${uidBytes.toHexString}")
        uidBytes
    }

    lazy val Uid: Long = {
        val uid = int64.decode(BitVector.view(UidBytes.toArray)).require.value
        println(s"UID: ${java.lang.Long.toUnsignedString(uid)}")
        uid
    }

    def isSkiTicket: Boolean = {
        nfc.memory(NfcTicket.TagPage) == NfcTicket.TagBytes
    }

    def readData(): Ticket = {
        readData(nfc.getCounter)
    }

    private def readData(currentCounter: Int): Ticket = {
        val codec =
            Ticket.codec(Uid, NfcTicket.authenticationKey(UidBytes), currentCounter)

        val size = (codec.sizeBound.exact.get.toInt + 7) / 8

        val sizeInPages = (size + NfcConstants.PageSize - 1) / NfcConstants.PageSize

        val startPage = NfcTicket.DataPage + sizeInPages * currentCounter % 2

        val bytes = nfc.memory(startPage, sizeInPages).slice(0, size)
        val data = codec.decode(BitVector.view(bytes.toArray)).require.value

        data
    }

    private def writeData(data: Ticket, counter: Int): Unit = {
        val nextCounter = counter + 1

        val codec = Ticket.codec(Uid, authenticationKey, nextCounter)

        val bits = codec.encode(data).require

        assert(Ticket.size == bits.length, s"Is size ${bits.size}, should be " +
                s"${Ticket.size}")

        val startPage = NfcTicket.startPage(nextCounter)

        nfc.memory(startPage, Ticket.sizeInPages).update(bits.toByteArray.toSeq)
        nfc.save()

        val writtenData = readData(nextCounter)
        check(data == writtenData, s"Mismatch between written and expect " +
                s"\nR $data\nW $writtenData\n")

        nfc.incrementCounter()
    }

    def check(cond: Boolean, s: String): Unit = {
        if (!cond) {
            throw NfcException(s)
        }
    }

    def format(): Unit = {
        println("Card needs formatting")
        check(nfc.utils.eraseMemory(), "Can't erase memory")

        check(nfc.utils.setAuth0(NfcTicket.TagPage), "Can't set auth0")
        check(nfc.utils.setAuth1(true), "Can't set auth1")

        println("Writing application tag.")
        nfc.memory(NfcTicket.TagPage, 1).update(NfcTicket.TagBytes)

        nfc.save()

        check(nfc.utils.changeKey(desAuthenticationKey.toArray), "Can't " +
                "change key")
        println("3DES key changed.")

        writeData(
            Ticket(
                Uid,
                0,
                List[Ticket.Subscription](None, None, None)
            ),
            nfc.getCounter
        )
    }

    def authenticate(): Unit = {
        if (!nfc.authenticate(NfcConstants.DefaultKey)) {
            println("3DES authentication with default key failed.")

            check(nfc.authenticate(desAuthenticationKey),
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

    def startPage(counter: Int): Int =
        NfcTicket.DataPage + Ticket.sizeInPages * counter % 2

    private def authenticationKey(uidBytes: Seq[Byte]): Seq[Byte] = {
        val sha256 = MessageDigest.getInstance("SHA-256")
        val uidDigest = sha256.digest(uidBytes.toArray)
        val masterKeyDigest = sha256.digest(NfcTicket.MasterKey.getBytes())

        sha256.update(uidDigest)
        val key = sha256.digest(masterKeyDigest).toSeq

        key
    }

    private def desAuthenticationKey(uidBytes: Seq[Byte]): Seq[Byte] =
        authenticationKey(uidBytes).slice(0, NfcConstants.KeySize)
}

