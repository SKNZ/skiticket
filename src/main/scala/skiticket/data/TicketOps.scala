package skiticket.data

import java.security.MessageDigest

import scodec.bits._
import scodec.codecs._
import skiticket.nfc.{NfcConstants, NfcException, NfcTools}
import skiticket.utils.ByteSeqExtension._

case class TicketOps(nfc: NfcTools) {
    /**
      * UID as bytes (8 bytes)
      */
    val uidBytes: Seq[Byte] = {
        val uidBytes = nfc.memory(NfcConstants.UidPage, NfcConstants.UidSizePages)
        println(s"UIDB: ${uidBytes.toHexString}")
        uidBytes
    }

    /**
      * UID as long
      */
    val uid: Long = {
        val uid = int64.decode(BitVector.view(uidBytes.toArray)).require.value
        println(s"UID: ${java.lang.Long.toUnsignedString(uid)}")
        uid
    }

    /**
      * Authentication key for 3DES authentication
      */
    val desAuthenticationKey = TicketOps.desAuthenticationKey(uidBytes)

    /**
      * Authentication key for HMAC
      */
    val hmacAuthenticationKey = TicketOps.HmacAuthenticationKey(uidBytes)

    /**
      * Checks for application tag
      *
      * @return true if tag present, false otherwise
      */
    def isSkiTicket: Boolean = {
        nfc.memory(TicketOps.TagPage) == TicketOps.TagBytes
    }

    /**
      * Read the ticket on the card (validates HMAC)
      *
      * @return ticket on the card
      */
    def read(): Ticket = {
        read(nfc.getCounter)
    }

    /**
      * Reads the ticket on the card (validates HMAC)
      *
      * @param currentCounter where to read the data
      * @return ticket as read
      */
    private def read(currentCounter: Int): Ticket = {
        // Create codec
        val codec = Ticket.fullCodec(hmacAuthenticationKey, currentCounter)

        val size = (codec.sizeBound.exact.get.toInt + 7) / 8

        val startPage = TicketOps.startPage(currentCounter)

        val bytes = nfc.memory(startPage, Ticket.sizeInPages).slice(0, size)

        val data = codec.decode(BitVector.view(bytes.toArray)).require.value

        data
    }

    /**
      * Write the ticket to the card, increment monotonic counter
      * Will reread before incrementing counter to validate write
      * Will also register write with ValidationLogger (write only happens
      * if validation was successful)
      *
      * @param data ticket to be writen
      */
    def write(data: Ticket): Unit = {
        val currentCounter = nfc.getCounter
        val nextCounter = currentCounter + 1

        val codec = Ticket.fullCodec(hmacAuthenticationKey, nextCounter)

        val bits = codec.encode(data).require

        assert(Ticket.size == bits.length, s"Is size ${bits.size}, should be " +
                s"${Ticket.size}")

        val startPage = TicketOps.startPage(nextCounter)
        println(s"Writing at page $startPage.")

        nfc.memory(startPage, Ticket.sizeInPages).update(bits.toByteArray.toSeq)
        nfc.save()

        val writtenData = read(nextCounter)
        if (writtenData != data) {
            throw new IllegalStateException(
                s"""Mismatch between written and expected
                   |R $data
                   |W $writtenData
                   |""".stripMargin)
        }

        if (TicketOps.TearingTest) {
            println(s"${Console.BOLD}Tearing test sleep now (1.0s)${Console.RESET}")
            Thread.sleep(1000)
        }

        nfc.incrementCounter()
        nfc.save()

        ValidationLogger.addPassage(LogEntry(uid, currentCounter))
    }

    /**
      * Like assert but throws NfcException instead
      *
      * @param condition    condition
      * @param errorMessage error message
      */
    def nfcAssert(condition: Boolean, errorMessage: String): Unit = {
        if (!condition) {
            throw NfcException(errorMessage)
        }
    }

    /**
      * Formats the card, must be authenticated before, will update key if
      * necessary
      */
    def format(): Unit = {
        println("Card needs formatting")
        // First reset the whole memory
        nfcAssert(nfc.utils.eraseMemory(), "Can't erase memory")

        // Set up card safety.
        // All pages should be unreadable and unwritable for unauthenticated
        nfcAssert(nfc.utils.setAuth0(TicketOps.TagPage), "Can't set auth0")
        nfcAssert(nfc.utils.setAuth1(true), "Can't set auth1")

        println("Writing application tag.")
        nfc.memory(TicketOps.TagPage, 1).update(TicketOps.TagBytes)

        // Write what's already done to the card
        // nfc.save()

        nfcAssert(
            nfc.utils.changeKey(desAuthenticationKey.toArray),
            "Can't change key"
        )
        println("3DES key changed.")

        // Write an empty ticket
        write(Ticket.empty)
    }

    /**
      * Authenticates with the card
      * Tries correct key then custom key.
      */
    def authenticate(): Unit = {
        if (!nfc.authenticate(desAuthenticationKey)) {
            println("3DES authentication with UID key failed.")

            nfcAssert(nfc.authenticate(NfcConstants.DefaultKey),
                "3DES authentication with default key failed.")
        }

        println("3DES authenticated.")
    }
}

object TicketOps {
    /**
      * Whether to pause during write for the tearing-test facility
      */
    var TearingTest = false

    /**
      * Page on which tag should be writen
      */
    val TagPage = 4

    /**
      * Page on which data should be writen
      */
    val DataPage = TagPage + 1

    /**
      * 3DES master key
      */
    private val DesMasterKey =
        "9gWwas51JqcG1zoRBgErqRRLdvIOkxS3H5WNO3wM6uRj982unYOxxm2eh3Vwvs1aOZM5kS2yKOAJKwPvw4zYjBu0krh5fMTLMdPq"

    /**
      * HMAC master key
      */
    private val HmacMasterKey =
        """bfLOgMznPK862Y8pKcmJ
          |z2UxAB1Wts5mv4aNtET0
          |vcpQNB9yk94PXzA1BbZR
          |hw5MYK0PAQF06cI8kxBf
          |yFrIiThnZ9G7NuI23Tde
          |dQd4xG0UwGvDO7CJHXBn
          |xXH6b7wOc54upnj8SOEc
          |RCYlgf1svQi7sBi4l1ma
          |6vG3rwJ5eVtMIC6TSApL
          |B1JxXM5z42ei6jBQ4yrl
          |""".stripMargin

    /**
      * Application tag
      */
    val TagBytes: Seq[Byte] = "SKI0".getBytes()

    /**
      * Page at which data should be written (according to left-right tear
      * protection)
      * @param counter indicator counter
      * @return page
      */
    def startPage(counter: Int): Int =
        TicketOps.DataPage + Ticket.sizeInPages * (counter % 2)

    /**
      * HMAC key
      * @param uidBytes uid bytes
      * @return key
      */
    private def HmacAuthenticationKey(uidBytes: Seq[Byte]): Seq[Byte] = {
        computeKey(uidBytes, TicketOps.HmacMasterKey)
    }

    /**
      * DES key
      * @param uidBytes uid bytes
      * @return key
      */
    private def desAuthenticationKey(uidBytes: Seq[Byte]): Seq[Byte] =
        computeKey(uidBytes, DesMasterKey).slice(0, NfcConstants.KeySize)

    private def computeKey(uidBytes: Seq[Byte], masterKey: String) = {
        val sha256 = MessageDigest.getInstance("SHA-256")
        val uidDigest = sha256.digest(uidBytes.toArray)
        val masterKeyDigest = sha256.digest(masterKey.getBytes())

        sha256.update(uidDigest)
        val key = sha256.digest(masterKeyDigest).toSeq

        key
    }
}

