package skiticket.nfc

import javax.smartcardio.CardException

import scodec.bits.BitVector
import scodec.codecs._
import skiticket.data.TicketOps
import skiticket.utils.ByteSeqExtension._
import skiticket.utils.LazyPagedArray
import ultralight.{CardReader, UltralightCommands, UltralightUtilities}

import scala.collection.mutable.ArrayBuffer

/**
  * An exception caused by the NFC tooling/card
  * @param reason error message
  */
case class NfcException(reason: String) extends Exception(reason)

/**
  * Tools used to simply NFC manipulation
  */
case class NfcTools() {
    val cardReader = new CardReader(System.out, null)

    if (!cardReader.initReader()) {
        throw NfcException("Can't init reader")
    }

    private var _utils: UltralightUtilities = _

    def utils: UltralightUtilities = _utils

    /**
      * Memory mapping with lazy loading
      */
    val memory: LazyPagedArray =
        LazyPagedArray(NfcConstants.MemorySize, NfcConstants.PageSize) { i =>
            ArrayBuffer(utils.readPage(i):_*)
        } {
            case (i, buffer) =>
                if (!utils.writePages(buffer.toArray, 0, i, 1)) {
                    throw NfcException("Can't write")
                }
        }

    // Initial connection
    reconnect()

    /**
      * (Re)connects to the card, discarding previous connection
      */
    def reconnect(): Unit = {
        if (!cardReader.initCard()) {
            throw NfcException("Can't get card")
        }

        val commands = new UltralightCommands(cardReader)
        _utils = new UltralightUtilities(commands, System.out)

        println(s"SAFE MODE: ${UltralightCommands.safe}")
    }

    /**
      * Write any pending state to the card
      */
    def save(): Unit = {
        memory.flush()
    }

    private var alreadyTriedAuth: Boolean = false

    /**
      * Authenticates with key.
      * @param key 3DES key
      * @return true if ok, false otherwise
      */
    def authenticate(key: Seq[Byte]): Boolean = {
        try {
            println(s"3DES trying with ${key.toHexString}.")

            if (alreadyTriedAuth) {
                println(s"NOTE: have to reconnect first.")
                if (!cardReader.initCard()) {
                    throw NfcException("Cant' get card")
                }
            } else {
                alreadyTriedAuth = true
            }

            utils.authenticate(key.toArray)
        } catch {
            case _: CardException => false
        }
    }

    private var counterCache: Option[Int] = None

    /**
      * Gets the monotonic counter
      * @return the monotonic counter value
      */
    def getCounter: Int = {
        if (counterCache.isEmpty) {
            val page = utils.readPage(NfcTools.CounterPage)

            counterCache = Some(uint32L.decode(BitVector(page))
                    .require
                    .value
                    .toInt)

            println(s"Read monotonic counter ${counterCache.get}.")
        }

        counterCache.get
    }

    /**
      * Increment monotonic counter
      */
    def incrementCounter(): Unit = {
        // Safe mode can't handle muh monotonic counter
        val buffer = uint32L.encode(if (UltralightCommands.safe) {
            getCounter + 1
        } else {
            1
        }).require.bytes.toArray

        if (!utils.writePages(buffer, 0, NfcTools.CounterPage, 1)) {
            throw NfcException("Can't write to monotonic counter")
        }
        // We want to reread counter after write
        counterCache = None
    }

    /**
      * Disconnects from card
      */
    def disconnect(): Unit = {
        cardReader.disconnect()
    }

    /**
      * Wait for card to be removed
      */
    def waitRemoved(): Unit = {
        cardReader.waitRemoved()
    }

}

object NfcTools {
    /**
      * Monotonic counter's page
      */
    val CounterPage = 41
}

