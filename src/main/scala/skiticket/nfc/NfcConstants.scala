package skiticket.nfc

object NfcConstants {
    val MemorySize = 192
    val PageSize = 4
    val PageCount = MemorySize / PageSize
    assert(PageCount * PageSize == MemorySize)

    val UidPage = 0
    val UidSizePages = 2

    val KeySize = 16
    val DefaultKey = "BREAKMEIFYOUCAN!".getBytes()
}
