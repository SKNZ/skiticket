package skiticket

import skiticket.nfc.NfcTools

object Main extends App {
    private val nfcTools = NfcTools()
    val nfcTicket = NfcTicket(nfcTools)

    nfcTicket.authenticate()

    if (true) {
        nfcTicket.format()
    }

    var ticket = nfcTicket.readData()
    println(ticket)
    ticket = ticket.issueRides(2)
    println(ticket)
    var validatedTicket = ticket.validate()
    println(s"VAL1: $validatedTicket")
    validatedTicket = validatedTicket.right.get.validate(true)
    println(s"VAL2: $validatedTicket")
    validatedTicket.right.get.validate(true)
    println(s"VAL3: $validatedTicket")
    validatedTicket = Right(validatedTicket.right.get.issueSubscription(1))
    validatedTicket = validatedTicket.right.get.validate(true)
    println(s"VAL4: $validatedTicket")
    validatedTicket = validatedTicket.right.get.validate(true)
    println(s"VAL5: $validatedTicket")

//    finalTicket.foreach(t => nfcTicket.writeData(t))
}
