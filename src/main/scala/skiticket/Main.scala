package skiticket

import skiticket.data.{TicketOps, ValidationLogger}
import skiticket.nfc.NfcTools

import scala.util.control.NonFatal

/**
  * Main object
  */
object Main extends App {
    if (args.length < 1
            || (args(0) == "issue" && args.length != 4)
            || (args(0) == "use" && args.length != 4)) {
        printUsage()
    }
    else {
        if (args(0) == "issue") {
            // Will throw if invalid format, good enough for demo
            val ridesNumber = args(1).toInt
            val subscription = args(2).toInt
            val format = args(3).toBoolean

            issue(ridesNumber, subscription, format)
        } else if (args(0) == "use") {
            val ignorePassback = args(1).toBoolean
            val blacklistFirstCard = args(2).toBoolean
            // This is static to enable tearing testing
            TicketOps.TearingTest = args(3).toBoolean

            use(ignorePassback, blacklistFirstCard)
        }
        else {
            printUsage()
        }

    }

    /**
      * Issues a ticket.
      *
      * @param ridesNumber  number of rides to be added
      * @param subscription subscription days to
      * @param format       force reset of the card before issuing
      */
    def issue(ridesNumber: Int, subscription: Int, format: Boolean): Unit = {
        val nfcTools = NfcTools()
        val ops = TicketOps(nfcTools)

        println("Authenticating")
        ops.authenticate()
        val isTicket = ops.isSkiTicket
        if (!isTicket || format) {
            println(s"Formatting was necessary: ${!isTicket}")
            ops.format()
        }

        // Read the ticket that's on the card
        val readTicket = ops.read()
        println(s"Read ticket: $readTicket")
        // Ticket object is immutable, use mutators to issue ticket
        val issuedTicket = readTicket.issueRides(ridesNumber).issueSubscription(subscription)
        println(s"Issued ticket: $issuedTicket")

        ops.write(issuedTicket)
        println("Done.")
    }

    /**
      * Validates tickets in a loop until killed.
      * @param ignorePassBack ignore pass back validation
      * @param blacklistFirstCard black list the first card that's presented
      */
    def use(ignorePassBack: Boolean, blacklistFirstCard: Boolean): Unit = {
        // Start fraud detector
        ValidationLogger.start()

        if (blacklistFirstCard) {
            gprintln("Present card to be blacklisted.")
            val tools = NfcTools()
            // Read first card presented and blacklist, we don't even care if
            // it's a proper ticket or not
            val ops = TicketOps(tools)
            ValidationLogger.blackList(ops.uid)
            gprintln("OK. Please remove card.")
            tools.waitRemoved()
        }

        while (true) {
            var valid = false
            var nfcTools: NfcTools = null

            try {
                nfcTools = NfcTools()
                val ops = TicketOps(nfcTools)
                ops.authenticate()

                if (!ops.isSkiTicket) {
                    rprintln("Result: not a ski ticket.")
                } else {
                    // Read the ticket
                    val ticket = ops.read()

                    // Validate (will decrement etc)
                    val validatedTicket = ticket.validate(ops.uid, ignorePassBack)

                    // If there is valid ticket, save the validated ticket
                    validatedTicket.right.foreach(x => {
                        ops.write(x)
                        valid = true
                    })

                    aprintln(valid, s"Result: $validatedTicket")
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
                        // We don't care about that
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

    // Prints bold
    def bprintln(x: Any) = println(s"${Console.BOLD}$x${Console.RESET}")

    // Prints yellow bold
    def oprintln(x: Any) = bprintln(s"${Console.YELLOW}$x")

    // Prints red bold
    def rprintln(x: Any) = bprintln(s"${Console.RED}$x")

    // Prints green
    def gprintln(x: Any) = bprintln(s"${Console.GREEN}$x${Console.RESET}")

    // Prints bold, green if true false otherwise
    def aprintln(y: Boolean, x: Any) = if (y) gprintln(x) else rprintln(x)

}

