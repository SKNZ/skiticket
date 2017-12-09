package skiticket.nfc

import javax.smartcardio.CardException

import org.scalatest._
import skiticket.NfcTicket

class NfcTicketTest extends fixture.FlatSpec with Matchers {
    // Initial setup
    {
        val tools = NfcTools()
        val ticket = NfcTicket(tools)
        ticket.authenticate()
        ticket.format()
        tools.disconnect()
    }

    case class FixtureParam(tools: NfcTools, ticket: NfcTicket) {
        def apply(f: (FixtureParam => Unit)*): Unit = {
            f.foreach(_ (this))
        }
    }

    def withFixture(test: OneArgTest): Outcome = {
        var nfcTools: NfcTools = null

        try {
            nfcTools = NfcTools()
            val nfcTicket = NfcTicket(nfcTools)
            val fixture = FixtureParam(nfcTools, nfcTicket)

            withFixture(test.toNoArgTest(fixture))
        } finally {
            if (nfcTools != null) {
                nfcTools.disconnect()
            }
        }
    }

    val authenticateFixture: FixtureParam => Unit = { f: FixtureParam =>
        f.ticket.authenticate()
    }

    val formatFixture: FixtureParam => Unit = { f: FixtureParam =>
        f.ticket.format()
    }

    // This amazing feat of programming is necessary because the card is
    // entirely reliable and deterministic
    def ffmc(f: FixtureParam => Any): FixtureParam => Any = {
        fp => {
            var o: Any = null
            var ok = false
            var i = 0

            while (!ok && i <= 5) {
                try {
                    o = f(fp)
                    ok = true
                }
                catch {
                    case _:NfcException | _:CardException =>
                        println("\n\n\n\n\nWAT U GON DO NFC ?\n\n\n\n\n")
                }
                i += 1
            }
            if (!ok) {
                o = f(fp)
            }
            o
        }
    }

    "Ticket" should "have application tag" in ffmc { f =>
        f(authenticateFixture, formatFixture)

        assert(f.ticket.isSkiTicket)
    }

    it should "have a non-default 3DES key" in ffmc { f =>
        assert(!f.tools.authenticate(NfcConstants.DefaultKey))
    }

    it should "have no value" in ffmc { f =>
        f(authenticateFixture, formatFixture)

        val ticket = f.ticket.readData()
        val refTicket = skiticket.Ticket(ticket.uid, 0, Seq(None, None, None))

        assert(ticket === refTicket)
    }

    it should "accept UID-key signed data" in ffmc { f =>

    }
}

