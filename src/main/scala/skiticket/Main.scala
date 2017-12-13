package skiticket

import skiticket.data.{NfcTicket, ValidationLogger}
import skiticket.nfc.NfcTools

import scala.util.control.NonFatal

object Main extends App {
    if (args.length < 1
            || (args(0) == "issue" && args.length != 4)
            || (args(0) == "use" && args.length != 4)) {
        printUsage()
    }
    else {
        if (args(0) == "issue") {
            val ridesNumber = args(1).toInt
            val subscription = args(2).toInt
            val format = args(3).toBoolean

            issue(ridesNumber, subscription, format)
        } else if (args(0) == "use") {
            val ignorePassback = args(1).toBoolean
            val blacklistFirstCard = args(2).toBoolean
            NfcTicket.TearingTest = args(3).toBoolean

            use(ignorePassback, blacklistFirstCard)
        }
        else {
            printUsage()
        }

    }

    def issue(ridesNumber: Int, subscription: Int, format: Boolean): Unit = {
        val nfcTools = NfcTools()
        val nfcTicket = NfcTicket(nfcTools)

        println("Authenticating")
        nfcTicket.authenticate()
        val isTicket = nfcTicket.isSkiTicket
        if (!isTicket || format) {
            println(s"Formatting (was necessary: ${!isTicket}")
            nfcTicket.format()
        }

        val readTicket = nfcTicket.readData()
        println(s"Read ticket: $readTicket")
        val issuedTicket = readTicket.issueRides(ridesNumber).issueSubscription(subscription)
        println(s"Issued ticket: $issuedTicket")

        nfcTicket.writeData(issuedTicket)
        println("Done.")
    }

    def use(ignorePassback: Boolean, blacklistFirstCard: Boolean): Unit = {
        ValidationLogger.start()
        if (blacklistFirstCard) {
            gprintln("Present card to be blacklisted.")
            val tools = NfcTools()
            val nfcTicket = NfcTicket(tools)
            ValidationLogger.blackList(nfcTicket.uid)
            gprintln("OK. Please remove card.")
            tools.waitRemoved()
        }

        while (true) {
            var auth = false
            var nfcTools: NfcTools = null
            try {
                nfcTools = NfcTools()
                val nfcTicket = NfcTicket(nfcTools)
                nfcTicket.authenticate()

                if (!nfcTicket.isSkiTicket) {
                    rprintln("Result: not a ski ticket.")
                } else {
                    val ticket = nfcTicket.readData()
                    val validatedTicket = ticket.validate(ignorePassback)
                    validatedTicket.right.foreach(x => {
                        nfcTicket.writeData(x)
                        auth = true
                    })

                    aprintln(auth, s"Result: $validatedTicket")
                }
            } catch {
                case NonFatal(e) =>
                    println(e)
                    e.printStackTrace()
            } finally {
                oprintln(s"Please remove the card.")
                try {
                    nfcTools.waitRemoved()
                } catch {
                    case e: Throwable =>
                }
            }
        }
        ValidationLogger.stop()
    }

    private def printUsage(): Unit = {
        println(
            """
USAGE:
issue <ridesNumber> <subscriptionLength> <forceFormat>
    Modifies the value on a single card before exiting.
    ridesNumber (int, 0-100) is the number of rides to be added
    subscriptionLength (int, 0-365) is the length of the subscription to be
                        added if there is already a subscription, a new one is
                        added (up to 3)
    forceFormat (true/false) will format/reset the card before writing to it
    ex: issue 10 0 true

use <ignorePassBack> <blacklistFirstCard> <tearingTest>
    Checks cards for validity until otherwise killed.
    ignorePassBack (true/false) will ignore last validation date for a card
    blacklistFirstCard (true/false) will make the first card presented be
                        blacklisted (until next launch). Two cards are then
                        recommended for testing.
    tearingTest (true/false) inserts a deliberate pause before monotonic
    counter write for the purpose of tearing testing
    ex: use true true false
""")
    }

    def bprintln(x: Any) = println(s"${Console.BOLD}$x${Console.RESET}")

    def oprintln(x: Any) = bprintln(s"${Console.YELLOW}$x")

    def rprintln(x: Any) = bprintln(s"${Console.RED}$x")

    def gprintln(x: Any) = bprintln(s"${Console.GREEN}$x${Console.RESET}")

    def aprintln(y: Boolean, x: Any) = if (y) gprintln(x) else rprintln(x)

}

