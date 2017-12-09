package skiticket

import java.time.{Duration, LocalDate, LocalDateTime}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scodec._
import scodec.codecs._
import skiticket.Ticket.Subscription
import skiticket.nfc.NfcConstants
import skiticket.utils.codecs._

object Errors extends Enumeration {

    protected case class Val(message: String) extends super.Val

    val PassBackProtection = Val("Pass back protection")
    val NoRidesOrSubscription = Val("No rides or subscriptions left")
}

case class Ticket(uid: Long,
                  ridesRemaining: Int,
                  subscriptions: Seq[Subscription],
                  lastValidation: LocalDateTime = Ticket.BaseDate) {
    require(ridesRemaining >= 0, "Negative number of rides")
    require(ridesRemaining <= Ticket.MaxNumberOfRides, "Too many rides")

    // Subscription validation
    {
        val maxDate: LocalDateTime =
            LocalDateTime.now().plusDays(Ticket.MaxSubscriptionLength)

        require(subscriptions.size <= Ticket.MaxNumberOfSubscriptions)
        subscriptions.foreach {
            case Some(Left(expiryDate: LocalDateTime)) =>
                require(maxDate.isAfter(expiryDate),
                    "Active subscription too long")

            case Some(Right(days: Int)) =>
                require(days > 0, "Null length subscription not allowed")
                require(days <= Ticket.MaxSubscriptionLength,
                    "Inactive subscription too long")

            case None =>
        }
    }

    def issueRides(rides: Int): Ticket = {
        copy(ridesRemaining = ridesRemaining + rides)
    }

    def issueSubscription(days: Int): Ticket = {
        val expiredIndex = subscriptions.zipWithIndex.collectFirst {
            case (Some(Left(expiry)), i) if expiry.isBefore(LocalDateTime.now()) =>
                i
            case (None, i) =>
                i
        }
        require(expiredIndex.isDefined)
        val newSubscriptions =
            subscriptions.updated(expiredIndex.get, Some(Right(days)))

        copy(subscriptions = newSubscriptions)
    }

    def validate(ignorePassBack: Boolean = false): Either[Errors.Value, Ticket] = {
        val timeDifference =
            Duration.between(lastValidation, LocalDateTime.now())

        // Pass back validation
        if (!ignorePassBack
                && timeDifference.compareTo(Ticket.AntiPassBackTimer) < 0) {
            Left(Errors.PassBackProtection)
        }
        else {
            // Do we have any already-activated valid subscriptions ?
            val activeSubscription = subscriptions.collectFirst {
                case Some(Left(expiry)) if expiry.isAfter(LocalDateTime.now()) =>
                    this
            }

            // Do we have any non-activated subscriptions ?
            val validSubscription = subscriptions.zipWithIndex.collectFirst {
                case (Some(Right(days)), i) =>
                    val newSubscriptions = subscriptions.updated(
                        i,
                        Some(Left(LocalDateTime.now().plusDays(days)))
                    )

                    copy(subscriptions = newSubscriptions)
            }

            // Do we have any rides remaining ?
            val ridesBased = if (ridesRemaining > 0) {
                Some(copy(ridesRemaining = ridesRemaining - 1))
            } else None

            activeSubscription
                    .orElse(validSubscription)
                    .orElse(ridesBased)
                    .map(t => t.copy(lastValidation = LocalDateTime.now()))
                    .map(t => {
                        t
                    })
                    .toRight(Errors.NoRidesOrSubscription)
        }
    }
}

object Ticket {
    type Subscription = Option[Either[LocalDateTime, Int]]

    val MaxNumberOfSubscriptions = 3
    val MaxNumberOfRides = 100
    val MaxSubscriptionLength = 365
    val AntiPassBackTimer: Duration = Duration.ofMinutes(1)

    val BaseDate: LocalDateTime = LocalDate.of(2017, 12, 3).atStartOfDay()

    private val subscriptionHoursCodec: Codec[Int] = uintL(31)
    private val dateTimeCodec =
        offsetDateTimeCodec(BaseDate, subscriptionHoursCodec)

    def codec(uid: Long, keyBytes: Seq[Byte], counter: Int): Codec[Ticket] = {
        val macAlgorithm = "HmacMD5"
        val key = new SecretKeySpec(keyBytes.toArray, macAlgorithm)
        val macInstance = Mac.getInstance(macAlgorithm)
        macInstance.init(key)

        val suffix = uint32L.encode(counter).require

        macCodec(macInstance, suffix)(("uid" | provide(uid)) :: dataCodec).as[Ticket]
    }

    val dataCodec = ("ridesRemaining" | uint16L) ::
            ("subscriptions" | knownSizeSeq(
                MaxNumberOfSubscriptions,
                presentIfNonZero(
                    either(
                        bool(1),
                        dateTimeCodec,
                        subscriptionHoursCodec
                    ),
                    Right(0)
                )
            )) ::
            ("lastValidation" | dateTimeCodec)

    val size: Int = codec(0, Array[Byte](), 0).sizeBound.exact.get.toInt
    val sizeInPages: Int = ((size + 7) / 8 + NfcConstants.PageSize - 1) / NfcConstants.PageSize
}

