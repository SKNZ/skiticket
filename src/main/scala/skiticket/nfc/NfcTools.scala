package skiticket.nfc

import javax.smartcardio.CardException

import scodec.bits.BitVector
import scodec.codecs._
import skiticket.NfcTicket
import skiticket.utils.ByteSeqExtension._
import skiticket.utils.LazyPagedArray
import ultralight.{CardReader, UltralightCommands, UltralightUtilities}

import scala.collection.mutable.ArrayBuffer

case class NfcTools() {
    val cardReader = new CardReader(System.out, null)
    assert(cardReader.initReader())

    private var _utils: UltralightUtilities = _

    def utils: UltralightUtilities = _utils

    val memory: LazyPagedArray =
        LazyPagedArray(NfcConstants.MemorySize, NfcConstants.PageSize) { i =>
            ArrayBuffer(utils.readPage(i):_*)
        } {
            case (i, buffer) =>
                assert(utils.writePages(buffer.toArray, 0, i, 1))
        }

    reconnect()

    def reconnect(): Unit = {
        assert(cardReader.initCard())

        val commands = new UltralightCommands(cardReader)
        _utils = new UltralightUtilities(commands, System.out)

        println(s"SAFE MODE: ${UltralightCommands.safe}")
    }

    def save(): Unit = {
        memory.flush()
    }

    private var alreadyTriedAuth: Boolean = false

    def authenticate(key: Seq[Byte]): Boolean = {
        try {
            println(s"3DES trying with ${key.toHexString}.")

            if (alreadyTriedAuth) {
                println(s"NOTE: have to reconnect first.")
                assert(cardReader.initCard())
            } else {
                alreadyTriedAuth = true
            }

            utils.authenticate(key.toArray)
        } catch {
            case _: CardException => false
        }
    }

    def getCounter: Int = {
        val page = utils.readPage(NfcTicket.CounterPage)

        uint32.decode(BitVector(page))
                .require
                .value
                .toInt
    }

    def incrementCounter(): Unit = {
        // Safe mode can't handle muh monotonic counter
        val buffer = uint32.encode(if (UltralightCommands.safe) {
            getCounter + 1
        } else {
            1
        }).require.bytes.toArray

        assert(utils.writePages(buffer, 0, NfcTicket.CounterPage, 1))
    }
}

