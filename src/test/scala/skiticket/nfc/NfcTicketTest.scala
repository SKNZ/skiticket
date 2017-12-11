package skiticket.nfc

import java.nio.ByteBuffer
import java.time.LocalDateTime
import javax.smartcardio.CardException

import org.scalatest._
import shapeless.Generic
import skiticket.data._
import skiticket.utils.ByteSeqExtension._

class NfcTicketTest extends fixture.FlatSpec with Matchers {
    // This amazing feat of programming is necessary because the card is
    // entirely reliable and deterministic
    def ffmc(f: FixtureParam => Any): FixtureParam => Any = {
        fp => {
            var o: Any = null
            var ok = false
            var i = 0

            while (!ok && i <= 5) {
                val tools = NfcTools()
                val ticket = NfcTicket(tools)

                try {
                    o = f(FixtureParam(tools, ticket))
                    ok = true
                }
                catch {
                    case _: NfcException | _: CardException =>
                        println("\n\n\n\n\nWAT U GON DO NFC ?\n\n\n\n\n")
                }
                finally {
                    tools.disconnect()
                }
                i += 1
            }
            if (!ok) {
                val tools = NfcTools()
                val ticket = NfcTicket(tools)

                try {
                    o = f(FixtureParam(tools, ticket))
                    ok = true
                }
                finally {
                    tools.disconnect()
                }
            }
            o
        }
    }

    // Initial setup
    ffmc { _ =>
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
            //            val fixture = FixtureParam(null, null)

            withFixture(test.toNoArgTest(null))
        } finally {
            if (nfcTools != null) {
                nfcTools.disconnect()
            }
        }
    }

    val authenticateFixture: FixtureParam => Unit = _.ticket.authenticate()

    val recoAuthFixture: FixtureParam => Unit = { f =>
        f.tools.reconnect()
        authenticateFixture(f)
    }

    val formatFixture: FixtureParam => Unit = _.ticket.format()

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
        val refTicket = Ticket(ticket.uid, ticket.counter, 0, Seq(None, None, None))

        assert(ticket === refTicket)
    }

    it should "fail to write data without auth" in ffmc { f =>
        val bytes = ByteBuffer.allocate(4).putInt(0xDEADBEEF).array()
        val writeOk = f.tools.utils.writePages(bytes, 0, NfcTicket.TagPage, 1)
        assert(writeOk)

        f(recoAuthFixture)
        assert(f.tools.memory(NfcTicket.TagPage) == NfcTicket.TagBytes)
    }

    it should "fail to read data without auth" in ffmc { f =>
        val bytes = new Array[Byte](4)
        val readOk = f.tools.utils.readPages(NfcTicket.DataPage, 1, bytes, 0)
        assert(!readOk)

        f(recoAuthFixture)
        assert(f.tools.memory(NfcTicket.TagPage) == NfcTicket.TagBytes)
    }

    it should "not validate incorrectly signed data" in ffmc { f =>
        f(authenticateFixture, formatFixture)

        val g = Generic[Ticket]
        val hList = g.to(Ticket(0, 0, 100, Seq(Some(Right(30)), None, None))).drop(2)
        val bytes = Ticket.dataCodec.encode(hList).require.toByteArray.toSeq

        val counter = f.tools.getCounter
        val startPage = NfcTicket.startPage(counter)
        f.tools.memory(startPage, Ticket.sizeInPages).update(bytes)
        f.tools.save()

        f(authenticateFixture)

        val e = intercept[IllegalArgumentException](f.ticket.readData())
        assert(e.getMessage.startsWith("MAC did not match"))
    }

    it should "add rides" in ffmc(writeRideFixture)

    it should "add subscription" in ffmc(writeSubscriptionFixture)

    it should "validate a ride" in ffmc { f =>
        val loadedTicket = writeRideFixture(f)

        val usedTicket = loadedTicket.validate().right.get
        assert(usedTicket.ridesRemaining == 0)
        f.ticket.writeData(usedTicket)

        f(recoAuthFixture)

        val readTicket = f.ticket.readData()
        assert(readTicket == usedTicket)
    }

    it should "activate/validate a subscription" in ffmc { f =>
        val dormantTicket = writeSubscriptionFixture(f)

        val activatedTicket = dormantTicket.validate().right.get
        assert(activatedTicket.subscriptions.head.get.left.get.isAfter(LocalDateTime.now()))
        f.ticket.writeData(activatedTicket)

        f(recoAuthFixture)

        val readTicket = f.ticket.readData()
        assert(readTicket == activatedTicket)

        val validatedTicket = readTicket.validate(true)
        assert(validatedTicket.isRight)
    }

    it should "validate subscription before ticket" in ffmc { f =>
        f(authenticateFixture, formatFixture)
        val emptyTicket = f.ticket.readData()

        val loadedTicket = emptyTicket.issueRides(1).issueSubscription(1)
        f.ticket.writeData(loadedTicket)

        f(recoAuthFixture)

        val readTicket = f.ticket.readData()
        val validatedTicket = readTicket.validate().right.get
        assert(validatedTicket.ridesRemaining == 1)
        assert(validatedTicket.subscriptions.head.get.isLeft)
        f.ticket.writeData(validatedTicket)

        f(recoAuthFixture)

        val revalidatedTicket = f.ticket.readData().validate(true).right.get
        assert(revalidatedTicket.ridesRemaining == 1)
        assert(revalidatedTicket.subscriptions.head.get.isLeft)
    }

    it should "not validate if no ride or subscription" in ffmc { f =>
        f(authenticateFixture, formatFixture)
        val ticket = f.ticket.readData()
        val result = ticket.validate()

        assert(result.left.get == Errors.NoRidesOrSubscription)
    }

    it should "not validate an expired subscription" in ffmc { f =>
        f(authenticateFixture, formatFixture)
        val ticket = f.ticket.readData()
        val oldTicket = ticket.copy(subscriptions = ticket.subscriptions
                .updated(0, Some(Left(LocalDateTime.now().minusDays(1).withNano(0)))))
        f.ticket.writeData(oldTicket)

        f(recoAuthFixture)

        val result = f.ticket.readData().validate()
        assert(result.left.get == Errors.NoRidesOrSubscription)
    }

    it should "check for passback" in ffmc { f =>
        f(authenticateFixture, formatFixture)
        val ticket = writeSubscriptionFixture(f)
        val validatedTicket = ticket.validate().right.get
        val revalidatedTicket = validatedTicket.validate()
        assert(revalidatedTicket.left.get == Errors.PassBackProtection)

        f.ticket.writeData(validatedTicket)

        f(recoAuthFixture)
        val readTicket = f.ticket.readData()
        val rerevalidatedTicket = readTicket.validate()
        println(
            s"""TICKET $ticket
               |VALTIC $validatedTicket
               |REVALT $revalidatedTicket
               |READTI $readTicket
               |REREVA $rerevalidatedTicket""".stripMargin)
        assert(rerevalidatedTicket.left.get == Errors.PassBackProtection)
    }

    it should "not accept nanoseconds in dates" in { f =>
        intercept[IllegalArgumentException](Ticket(
            0,
            0,
            0,
            Seq(Some(Left(LocalDateTime.now().withNano(0))), None, None),
            LocalDateTime.now().withNano(1)
        ))

        intercept[IllegalArgumentException](Ticket(
            0,
            0,
            0,
            Seq(Some(Left(LocalDateTime.now())), None, None), LocalDateTime.now().withNano(0)
        ))
    }

    it should "blacklist a uid/counter pair who appears twice" in { _ =>
        ValidationLogger.start()

        ValidationLogger.addPassage(LogEntry(2, 1))
        ValidationLogger.addPassage(LogEntry(2, 2))
        ValidationLogger.addPassage(LogEntry(2, 3))
        ValidationLogger.addPassage(LogEntry(1, 1))
        ValidationLogger.addPassage(LogEntry(1, 1))
        Thread.sleep(1000)
        ValidationLogger.stop()
        assert(ValidationLogger.isBlacklisted(1))
        assert(!ValidationLogger.isBlacklisted(2))

        val ticket = Ticket(1, 1, 0, Seq(None, None, None))
        val validated = ticket.validate()
        assert(validated.left.get == Errors.BlackListed)
    }

    private def writeRideFixture(f: FixtureParam) = {
        f(authenticateFixture, formatFixture)

        val emptyTicket = f.ticket.readData()
        val issuedTicket = emptyTicket.issueRides(1)
        assert(issuedTicket.ridesRemaining == 1)
        f.ticket.writeData(issuedTicket)

        f(recoAuthFixture)

        val loadedTicket = f.ticket.readData()
        assert(loadedTicket == issuedTicket)
        loadedTicket
    }

    private def writeSubscriptionFixture(f: FixtureParam) = {
        f(authenticateFixture, formatFixture)

        val emptyTicket = f.ticket.readData()
        val issuedTicket = emptyTicket.issueSubscription(30)
        assert(issuedTicket.subscriptions.head.get.right.get == 30)
        assert(issuedTicket.ridesRemaining == 0)
        f.ticket.writeData(issuedTicket)

        f(recoAuthFixture)

        val loadedTicket = f.ticket.readData()
        assert(loadedTicket == issuedTicket)
        loadedTicket
    }
}

