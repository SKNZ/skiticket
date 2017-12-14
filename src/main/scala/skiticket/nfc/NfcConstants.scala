package skiticket.nfc

object NfcConstants {
    /**
      * Size of memory
      */
    val MemorySize = 192
    /**
      * Size of page
      */
    val PageSize = 4
    /**
      * Number of pages
      */
    val PageCount = MemorySize / PageSize
    assert(PageCount * PageSize == MemorySize)

    /**
      * Pages where UID is stored
      */
    val UidPage = 0
    /**
      * Number of pages on which UID is stored
      */
    val UidSizePages = 2

    /**
      * Size of DES key in bytes
      */
    val KeySize = 16

    /**
      * Default DES key
      */
    val DefaultKey = "BREAKMEIFYOUCAN!".getBytes()
}
