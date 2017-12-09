package skiticket.utils

import scodec.bits.ByteVector
import skiticket.nfc.NfcException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class LazyPagedArray(size: Int, pageSize: Int)
                         (pageLoader: Int => Seq[Byte])
                         (pageFlusher: (Int, Seq[Byte]) => Unit) {
    private val pageCount = size / pageSize
    require(pageCount * pageSize == size)

    private val pages: ArrayBuffer[ArrayBuffer[Byte]] =
        ArrayBuffer.fill(pageCount)(new ArrayBuffer[Byte]())

    private val loadedPages: mutable.BitSet = new mutable.BitSet(pageCount)
    private val dirtyPages: mutable.BitSet = new mutable.BitSet(pageCount)

    case class PageWrapper(firstPage: Int, lastPage: Int)
            extends mutable.IndexedSeq[Byte] {
        override def update(idx: Int, elem: Byte): Unit = {
            require(idx <= length)
            val pageNumber = firstPage + idx / pageSize
            val pageIdx = idx % pageSize
            dirtyPages += pageNumber
            pages(pageNumber).update(pageIdx, elem)
        }

        override def length: Int = pageSize * (lastPage - firstPage)

        override def apply(idx: Int): Byte = {
            require(idx <= length)
            val pageNumber = firstPage + idx / pageSize
            val pageIdx = idx % pageSize
            pages(pageNumber)(pageIdx)
        }
    }

    def apply(): mutable.IndexedSeq[Byte] = PageWrapper(0, pageCount)

    def apply(pageNumber: Int, pageCount: Int): mutable.IndexedSeq[Byte] = {
        loadPages(pageNumber, pageCount)

        PageWrapper(pageNumber, pageNumber + pageCount)
    }

    def apply(pageNumber: Int): mutable.IndexedSeq[Byte] = {
        apply(pageNumber, 1)
    }

    def flush(): Unit = {
        for (pageNumber <- dirtyPages) {
            pageFlusher(pageNumber, pages(pageNumber))
        }

        dirtyPages.clear()
        loadedPages.clear()
        pages.foreach(_.clear())
    }

    private def loadPages(pageNumber: Int, pageCount: Int): Unit = {
        val pageNumbers = Range(pageNumber, pageNumber + pageCount)
        val pagesSet = mutable.BitSet(pageNumbers: _*)
        val pagesToBeLoaded = pagesSet -- loadedPages

        pagesToBeLoaded.foreach { currentPageNumber =>
            require(currentPageNumber != 41, "Use dedicated functions to " +
                    "manipulate monotonic counter")
            val bytes = pageLoader(currentPageNumber)

            pages(currentPageNumber) ++= bytes

            loadedPages += currentPageNumber
        }

        if (!pagesToBeLoaded.subsetOf(loadedPages)) {
            throw new RuntimeException("Missing pages in loaded set")
        }
    }
}

