package skiticket.nfc

import java.nio.ByteBuffer
import java.time.LocalDateTime
import javax.smartcardio.CardException

import org.scalatest._
import shapeless.Generic
import skiticket.data._
import skiticket.utils.ByteSeqExtension._

class TicketOpsTest extends FlatSpec with Matchers {
    // Initial setup
    nfcSafeExec { _ =>
        val tools = NfcTools()
        val ticket = TicketOps(tools)
        ticket.authenticate()
        ticket.format()
        tools.disconnect()
    }

    case class FixtureParam(tools: NfcTools, ops: TicketOps) {
        def apply(f: (FixtureParam => Unit)*): Unit = {
            f.foreach(_ (this))
        }
    }

    val authenticateFixture: FixtureParam => Unit = _.ops.authenticate()

    val recoAuthFixture: FixtureParam => Unit = { f =>
        f.tools.reconnect()
        authenticateFixture(f)
    }

    val formatFixture: FixtureParam => Unit = _.ops.format()

    "Ticket" should "have application tag" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)

        assert(f.ops.isSkiTicket)
    }

    it should "have a non-default 3DES key" in nfcSafeExec { f =>
        assert(!f.tools.authenticate(NfcConstants.DefaultKey))
    }

    it should "have no value" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)

        val ticket = f.ops.read()
        val refTicket = Ticket(0, Seq(None, None, None))

        assert(ticket === refTicket)
    }

    it should "fail to write data without auth" in nfcSafeExec { f =>
        val bytes = ByteBuffer.allocate(4).putInt(0xDEADBEEF).array()
        val writeOk = f.tools.utils.writePages(bytes, 0, TicketOps.TagPage, 1)
        assert(writeOk)

        f(recoAuthFixture)
        assert(f.tools.memory(TicketOps.TagPage) == TicketOps.TagBytes)
    }

    it should "fail to read data without auth" in nfcSafeExec { f =>
        val bytes = new Array[Byte](4)
        val readOk = f.tools.utils.readPages(TicketOps.DataPage, 1, bytes, 0)
        assert(!readOk)

        f(recoAuthFixture)
        assert(f.tools.memory(TicketOps.TagPage) == TicketOps.TagBytes)
    }

    it should "not validate incorrectly signed data" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)

        val fakeTicket = Ticket(100, Seq(Some(Right(30)), None, None))
        val bytes = Ticket.dataCodec.encode(fakeTicket).require.toByteArray.toSeq

        val counter = f.tools.getCounter
        val startPage = TicketOps.startPage(counter)
        f.tools.memory(startPage, Ticket.sizeInPages).update(bytes)
        f.tools.save()

        f(authenticateFixture)

        val e = intercept[IllegalArgumentException](f.ops.read())
        assert(e.getMessage.startsWith("MAC did not match"))
    }

    it should "add rides" in nfcSafeExec(writeRideFixture)

    it should "add subscription" in nfcSafeExec(writeSubscriptionFixture)

    it should "validate a ride" in nfcSafeExec { f =>
        val loadedTicket = writeRideFixture(f)

        val usedTicket = loadedTicket.validate(f.ops.uid).right.get
        assert(usedTicket.ridesRemaining == 0)
        f.ops.write(usedTicket)

        f(recoAuthFixture)

        val readTicket = f.ops.read()
        assert(readTicket == usedTicket)
    }

    it should "activate/validate a subscription" in nfcSafeExec { f =>
        val dormantTicket = writeSubscriptionFixture(f)

        val activatedTicket = dormantTicket.validate(f.ops.uid).right.get
        assert(activatedTicket.subscriptions.head.get.left.get.isAfter(LocalDateTime.now()))
        f.ops.write(activatedTicket)

        f(recoAuthFixture)

        val readTicket = f.ops.read()
        assert(readTicket == activatedTicket)

        val validatedTicket = readTicket.validate(f.ops.uid)
        assert(validatedTicket.isRight)
    }

    it should "validate subscription before ticket" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)
        val emptyTicket = f.ops.read()

        val loadedTicket = emptyTicket.issueRides(1).issueSubscription(1)
        f.ops.write(loadedTicket)

        f(recoAuthFixture)

        val readTicket = f.ops.read()
        val validatedTicket = readTicket.validate(f.ops.uid).right.get
        assert(validatedTicket.ridesRemaining == 1)
        assert(validatedTicket.subscriptions.head.get.isLeft)
        f.ops.write(validatedTicket)

        f(recoAuthFixture)

        val revalidatedTicket = f.ops.read().validate(f.ops.uid).right.get
        assert(revalidatedTicket.ridesRemaining == 1)
        assert(revalidatedTicket.subscriptions.head.get.isLeft)
    }

    it should "not validate if no ride or subscription" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)
        val ticket = f.ops.read()
        val result = ticket.validate(f.ops.uid)

        assert(result.left.get == ValidationErrors.NoRidesOrSubscription)
    }

    it should "not validate an expired subscription" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)
        val ticket = f.ops.read()
        val oldTicket = ticket.copy(subscriptions = ticket.subscriptions
                .updated(0, Some(Left(LocalDateTime.now().minusDays(1).withNano(0)))))
        f.ops.write(oldTicket)

        f(recoAuthFixture)

        val result = f.ops.read().validate(f.ops.uid)
        assert(result.left.get == ValidationErrors.NoRidesOrSubscription)
    }

    it should "check for passback" in nfcSafeExec { f =>
        f(authenticateFixture, formatFixture)
        val ticket = writeSubscriptionFixture(f)
        val validatedTicket = ticket.validate(f.ops.uid).right.get
        val revalidatedTicket = validatedTicket.validate(f.ops.uid)
        assert(revalidatedTicket.left.get == ValidationErrors.PassBackProtection)

        f.ops.write(validatedTicket)

        f(recoAuthFixture)
        val readTicket = f.ops.read()
        val rerevalidatedTicket = readTicket.validate(f.ops.uid)
        println(
            s"""TICKET $ticket
               |VALTIC $validatedTicket
               |REVALT $revalidatedTicket
               |READTI $readTicket
               |REREVA $rerevalidatedTicket""".stripMargin)
        assert(rerevalidatedTicket.left.get == ValidationErrors.PassBackProtection)
    }

    it should "not accept nanoseconds in dates" in {
        intercept[IllegalArgumentException](Ticket(
            0,
            Seq(Some(Left(LocalDateTime.now().withNano(0))), None, None),
            LocalDateTime.now().withNano(1)
        ))

        intercept[IllegalArgumentException](Ticket(
            0,
            Seq(Some(Left(LocalDateTime.now())), None, None),
            LocalDateTime.now().withNano(0)
        ))
    }

    it should "blacklist a uid/counter pair who appears twice" in {
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
    }

    private def writeRideFixture(f: FixtureParam) = {
        f(authenticateFixture, formatFixture)

        val emptyTicket = f.ops.read()
        val issuedTicket = emptyTicket.issueRides(1)
        assert(issuedTicket.ridesRemaining == 1)
        f.ops.write(issuedTicket)

        f(recoAuthFixture)

        val loadedTicket = f.ops.read()
        assert(loadedTicket == issuedTicket)
        loadedTicket
    }

    private def writeSubscriptionFixture(f: FixtureParam) = {
        f(authenticateFixture, formatFixture)

        val emptyTicket = f.ops.read()
        val issuedTicket = emptyTicket.issueSubscription(30)
        assert(issuedTicket.subscriptions.head.get.right.get == 30)
        assert(issuedTicket.ridesRemaining == 0)
        f.ops.write(issuedTicket)

        f(recoAuthFixture)

        val loadedTicket = f.ops.read()
        assert(loadedTicket == issuedTicket)
        loadedTicket
    }

    // This amazing feat of programming is necessary because the card is
    // entirely reliable and deterministic
    // Note that the problems this "solves" only appear when unit testing
    def nfcSafeExec(f: FixtureParam => Any): FixtureParam => Any = {
        fp => {
            var o: Any = null
            var ok = false
            var i = 0

            while (!ok && i <= 5) {
                val tools = NfcTools()
                val ticket = TicketOps(tools)

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
                val ticket = TicketOps(tools)

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
}

