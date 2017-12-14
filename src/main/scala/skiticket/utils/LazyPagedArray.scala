package skiticket.utils

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Array of pages, lazy loads pages, maintains list of dirty pages
  *
  * @param size        number of pages
  * @param pageSize    page size in bytes
  * @param pageLoader  page loading function
  * @param pageFlusher page flushing function
  */
case class LazyPagedArray(size: Int, pageSize: Int)
                         (pageLoader: Int => Seq[Byte])
                         (pageFlusher: (Int, Seq[Byte]) => Unit) {
    private val pageCount = size / pageSize
    require(pageCount * pageSize == size)

    /**
      * Array of array of bytes (i.e. array of pages)
      */
    private val pages: ArrayBuffer[ArrayBuffer[Byte]] =
        ArrayBuffer.fill(pageCount)(new ArrayBuffer[Byte]())

    /**
      * Loaded pages
      */
    private val loadedPages: mutable.BitSet = new mutable.BitSet(pageCount)
    /**
      * Dirty pages
      */
    private val dirtyPages: mutable.BitSet = new mutable.BitSet(pageCount)

    /**
      * Wraps multiple pages as continuous mutable indexed sequence
      *
      * @param firstPage first page
      * @param lastPage  last page (exclusive)
      */
    case class PageWrapper(firstPage: Int, lastPage: Int)
            extends mutable.IndexedSeq[Byte] {
        override def update(idx: Int, elem: Byte): Unit = {
            require(idx <= length)
            // Compute the page on which the byte is
            val pageNumber = firstPage + idx / pageSize
            // Compute the index of the byte in the page
            val pageIdx = idx % pageSize
            // Mark dirty
            dirtyPages += pageNumber
            pages(pageNumber).update(pageIdx, elem)
        }

        override def length: Int = pageSize * (lastPage - firstPage)

        override def apply(idx: Int): Byte = {
            require(idx <= length)
            // Compute page number
            val pageNumber = firstPage + idx / pageSize
            // Compute index of byte in page
            val pageIdx = idx % pageSize
            pages(pageNumber)(pageIdx)
        }
    }

    def apply(): mutable.IndexedSeq[Byte] = PageWrapper(0, pageCount)

    /**
      * Sequence of page
      *
      * @param pageNumber first page
      * @param pageCount  number of pages to load
      * @return indexed sequence of bytes from pages
      */
    def apply(pageNumber: Int, pageCount: Int): mutable.IndexedSeq[Byte] = {
        loadPages(pageNumber, pageCount)

        PageWrapper(pageNumber, pageNumber + pageCount)
    }

    /**
      * Gets page
      *
      * @param pageNumber page number
      * @return indexed sequence of page bytes
      */
    def apply(pageNumber: Int): mutable.IndexedSeq[Byte] = {
        apply(pageNumber, 1)
    }

    /**
      * Flushes all dirty pages
      */
    def flush(): Unit = {
        for (pageNumber <- dirtyPages) {
            pageFlusher(pageNumber, pages(pageNumber))
        }

        dirtyPages.clear()
        loadedPages.clear()
        pages.foreach(_.clear())
    }

    /**
      * Page loading helper
      * @param pageNumber start page
      * @param pageCount number of pages
      */
    private def loadPages(pageNumber: Int, pageCount: Int): Unit = {
        val pageNumbers = Range(pageNumber, pageNumber + pageCount)
        val pagesSet = mutable.BitSet(pageNumbers: _*)
        // Don't load already loaded pages
        val pagesToBeLoaded = pagesSet -- loadedPages

        pagesToBeLoaded.foreach { currentPageNumber =>
            require(currentPageNumber != 41, "Use dedicated functions to " +
                    "manipulate monotonic counter")

            val bytes = pageLoader(currentPageNumber)

            pages(currentPageNumber) ++= bytes

            loadedPages += currentPageNumber
        }

        assert(pagesToBeLoaded.subsetOf(loadedPages),
            "Missing pages in loaded set")
    }
}

