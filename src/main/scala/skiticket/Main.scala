package skiticket

import skiticket.nfc.NfcTools

object Main extends App {
    private val tools = NfcTools()
    val ticket = NfcTicket(tools)

    ticket.authenticate()

    if (!ticket.isSkiTicket) {
        ticket.format()
    }
}
