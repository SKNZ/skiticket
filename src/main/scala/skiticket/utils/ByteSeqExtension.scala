package skiticket.utils

import skiticket.nfc.NfcConstants

import scala.collection.mutable

/**
  * Extensions for Seq[Byte] and related manipulation with Array[Byte]
  */
object ByteSeqExtension {

    class RichByteSeq(val seq: Seq[Byte]) extends AnyVal {
        def toHexString: String = {
            seq.map("%02X".format(_))
                    .grouped(NfcConstants.PageSize)
                    .map(_.mkString + " ")
                    .mkString
        }
    }

    class RichMutableByteSeq(val me: mutable.IndexedSeq[Byte]) extends AnyVal {
        def update(newBytes: Seq[Byte]): Unit = {
            require(newBytes.length <= me.length, s"Source was ${me.length} " +
                    s"and new was ${newBytes.length}")

            newBytes.indices.foreach(i => me.update(i, newBytes(i)))
        }
    }

    implicit def richMutableByteSeq(seq: mutable.IndexedSeq[Byte]): RichMutableByteSeq = new RichMutableByteSeq(seq)

    implicit def richByteSeq(seq: Seq[Byte]): RichByteSeq =
        new RichByteSeq(seq)
}
