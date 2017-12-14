package skiticket.data

import java.time.{Duration, LocalDate, LocalDateTime}
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import scodec._
import scodec.codecs._
import shapeless.HNil
import skiticket.data.Ticket.Subscription
import skiticket.nfc.NfcConstants
import skiticket.utils.codecs._

/**
  * Possible validation errors
  */
object ValidationErrors extends Enumeration {
    val PassBackProtection = Val("Pass back protection")
    val NoRidesOrSubscription = Val("No rides or subscriptions left")
    val BlackListed = Val("Your UID is blacklisted")

    protected case class Val(message: String) extends super.Val

}

/**
  * Contents of a Ticket. Immutable.
  *
  * @param ridesRemaining number of rides remaining on card
  * @param subscriptions  hours-based subscriptions
  * @param lastValidation last validation time
  */
case class Ticket(ridesRemaining: Int,
                  subscriptions: Seq[Subscription],
                  lastValidation: LocalDateTime = Ticket.TicketEpoch) {
    require(ridesRemaining >= 0, "Negative number of rides")
    require(ridesRemaining <= Ticket.MaxNumberOfRides, "Too many rides")
    require(lastValidation.getNano == 0)

    // Subscription sanity checking
    {
        val maxDate: LocalDateTime =
            LocalDateTime.now().plusDays(Ticket.MaxSubscriptionLength)

        require(subscriptions.lengthCompare(Ticket.MaxNumberOfSubscriptions) == 0)
        subscriptions.foreach {
            case Some(Left(expiryDate: LocalDateTime)) =>
                require(expiryDate.getNano == 0)
                require(maxDate.isAfter(expiryDate),
                    "Active subscription too long")

            case Some(Right(hours: Int)) =>
                require(hours > 0, "Null length subscription not allowed")
                require(hours <= Ticket.MaxSubscriptionLength,
                    "Inactive subscription too long")

            case None =>
        }
    }

    /**
      * Issues a copy of this ticket with added number of rides.
      *
      * @param rides number of rides to add
      * @return ticket with added rides
      */
    def issueRides(rides: Int): Ticket = {
        copy(ridesRemaining = ridesRemaining + rides)
    }

    /**
      * Issues a copy of this ticket with new subscription.
      * New subscription will take empty spot or replace expired one.
      * Throws if no room.
      *
      * @param hours number of hours of new subscription. If 0, nothing done.
      * @return ticket with added subscription
      */
    def issueSubscription(hours: Int): Ticket = {
        if (hours != 0) {
            // Find index of expired subscription or unused spot
            val expiredIndex = subscriptions.zipWithIndex.collectFirst {
                case (Some(Left(expiry)), i) if expiry.isBefore(LocalDateTime.now()) =>
                    i
                case (None, i) =>
                    i
            }

            require(expiredIndex.isDefined)
            val newSubscriptions =
                subscriptions.updated(expiredIndex.get, Some(Right(hours)))

            copy(subscriptions = newSubscriptions)
        } else this
    }

    /**
      * Validates a ticket. Prioritizes time-based subscriptions over rides.
      * Activates a dormant subscription if available and not other
      * subscription is in use.
      * Checks for blacklists. Checks for pass back.
      *
      * @param uid            uid for blacklist checking
      * @param ignorePassBack disable pass back validation
      * @return If valid, validated ticket as Right, otherwise Error as Left
      */
    def validate(uid: Long, ignorePassBack: Boolean = false): Either[ValidationErrors.Value, Ticket] = {
        val timeDifference =
            Duration.between(lastValidation, LocalDateTime.now())

        if (ValidationLogger.isBlacklisted(uid)) {
            Left(ValidationErrors.BlackListed)
        }
        // Pass back validation
        else if (!ignorePassBack
                && timeDifference.compareTo(Ticket.AntiPassBackTimer) < 0) {
            Left(ValidationErrors.PassBackProtection)
        }
        else {
            // Do we have any already-activated valid subscriptions ?
            val activeSubscription = subscriptions.collectFirst {
                case Some(Left(expiry)) if expiry.isAfter(LocalDateTime.now()) =>
                    this
            }

            // Do we have any non-activated subscriptions ?
            val validSubscription = subscriptions.zipWithIndex.collectFirst {
                case (Some(Right(hours)), i) =>
                    val newSubscriptions = subscriptions.updated(
                        i,
                        Some(Left(LocalDateTime.now().plusHours(hours).withNano(0)))
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
                    .map(t => t.copy(lastValidation = LocalDateTime.now().withNano(0)))
                    .toRight(ValidationErrors.NoRidesOrSubscription)
        }
    }

    override def toString: String = {
        s"""Ticket{
    Rides remaining: $ridesRemaining
    Sub0: ${subscriptions(0)}
    Sub1: ${subscriptions(1)}
    Sub2: ${subscriptions(2)}
    Last validation: $lastValidation
}"""
    }
}

object Ticket {
    /**
      * A subscription is optionally present in the form either a date & time
      * if activated or in the form of a number of hours if dormant
      */
    type Subscription = Option[Either[LocalDateTime, Int]]

    val MaxNumberOfSubscriptions = 3
    val MaxNumberOfRides = 100
    val MaxSubscriptionLength = 365 * 24
    val AntiPassBackTimer: Duration = Duration.ofMinutes(1)

    val TicketEpoch: LocalDateTime = LocalDate.of(2017, 12, 3).atStartOfDay()

    private val subscriptionHoursCodec: Codec[Int] = uintL(31)
    private val dateTimeCodec =
        offsetDateTimeCodec(TicketEpoch, subscriptionHoursCodec)

    /**
      * Constructs a codec for the Ticket, including Mac.
      * @param hmacKeyBytes hmac key bytes
      * @param counter counter to use in mac (should be current or current+1)
      * @return ticket codec with mac validation
      */
    def fullCodec(hmacKeyBytes: Seq[Byte], counter: Int): Codec[Ticket] = {
        val macAlgorithm = "HmacMD5"
        val key = new SecretKeySpec(hmacKeyBytes.toArray, macAlgorithm)
        val macInstance = Mac.getInstance(macAlgorithm)
        macInstance.init(key)

        val suffixCodec = uint32L
        val suffix = counter.toLong
        val suffixBytes = suffixCodec.encode(suffix).require

        macCodec(macInstance, suffixBytes)(dataCodec)
    }

    val dataCodec: Codec[Ticket] = {
        // uid is set as "provide" because we don't want to rewrite it
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

    val size: Int = fullCodec(new Array[Byte](16), 0).sizeBound.exact.get.toInt
    val sizeInPages: Int = ((size + 7) / 8 + NfcConstants.PageSize - 1) / NfcConstants.PageSize

    val empty = Ticket(
        0,
        List[Ticket.Subscription](None, None, None)
    )
}

