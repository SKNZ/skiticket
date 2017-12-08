package skiticket

import java.time.{Duration, LocalDate, LocalDateTime}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scodec._
import scodec.codecs._
import skiticket.Ticket.Subscription
import skiticket.utils.codecs._

object Errors extends Enumeration {

    protected case class Val(message: String) extends super.Val

    val PassBackProtection = Val("Pass back protection")
    val NoRidesOrSubscription = Val("No rides or subscriptions left")
}

case class Ticket(var ridesRemaining: Int,
                  var subscriptions: Seq[Subscription],
                  var lastValidation: LocalDateTime) {
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
        copy(subscriptions = subscriptions.updated(expiredIndex.get, Some(Right(days))))
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
                    .toRight(Errors.NoRidesOrSubscription)
        }
    }
}

object Ticket {
    type Subscription = Option[Either[LocalDateTime, Int]]

    val MaxNumberOfRides = 100
    val MaxSubscriptionLength = 365
    val AntiPassBackTimer: Duration = Duration.ofMinutes(1)

    val BaseDate: LocalDateTime = LocalDate.of(2017, 12, 3).atStartOfDay()

    def codec(keyBytes: Seq[Byte], counter: Int): Codec[Ticket] = {
        val subscriptionHoursCodec = uintL(32)

        val dateTimeCodec = offsetDateTimeCodec(BaseDate, subscriptionHoursCodec)

        val macAlgorithm = "HmacMD5"
        val key = new SecretKeySpec(keyBytes.toArray, macAlgorithm)
        val macInstance = Mac.getInstance(macAlgorithm)
        macInstance.init(key)

        macCodec(macInstance, uint32L.encode(counter).require) {
            ("ridesRemaining" | uint16L) ::
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
        }.as[Ticket]
    }
}
