package skiticket.data

import java.time.{LocalDateTime, ZonedDateTime}
import java.util
import java.util.concurrent.atomic.AtomicInteger

import scalikejdbc._

import scala.collection.JavaConverters._

/**
  * Log entry
  *
  * @param uid     uid of card
  * @param counter monotonic counter value
  * @param time    time of validation
  */
case class LogEntry(uid: Long, counter: Int, time: LocalDateTime = LocalDateTime.now())

/**
  * Logs all validations to database in order to detect fraud.
  * Runs in its own thread.
  */
object ValidationLogger {
    // Disable verbose output from ORM
    GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
        enabled = false,
        singleLineMode = true,
        printUnprocessedStackTrace = false
    )
    // Load DB driver
    Class.forName("org.h2.Driver")
    // Setup in-memory connection
    ConnectionPool.singleton("jdbc:h2:mem:hello", "user", "pass")

    // Obtain database session with automatic commit
    implicit val session = DB.autoCommitSession()

    // Returns whole log
    def getAll: List[(Long, Int, ZonedDateTime)] = {
        sql"select * from passages".map(rs => (rs.long(1), rs.int(2), rs.dateTime(3))).list.apply
    }

    // Handler thread for received log entries
    private val receiverThread: Thread = new Thread(() => {
        // List of items to be processed
        val items = new util.ArrayList[LogEntry]()

        while (!Thread.currentThread().isInterrupted) {
            //            println("Validation logger flush")
            passagesQueue.drainTo(items)

            // Batch query params for prepared statement
            val queryParams = items.asScala.map(x => Seq(x.uid, x.counter, x
                    .time.toString))

            // Run statement
            sql"insert into passages values (?, ?, ?)"
                    .batch(queryParams: _*).apply()

            // Find all blacklisted uid, by find those who have more recent
            // entries with <= counter
            val blackListed =
            sql"""
select p1.id, p1.counter as p1c, p1.time as p1t, p2.counter as p2c, p2.time as p2t
 from passages p1
 join passages p2 on p1.id = p2.id and p1.counter <= p2.counter
 where p1.time > p2.time
            """.map(rs => rs.toMap()).list.apply

            if (blackListed.nonEmpty) {
                println(s"Found fraud, will blacklist because: $blackListed")

                // Add blacklisted UIDs
                blacklist ++= blackListed.map(x => x("ID").toString.toLong)
            }

            items.clear()
            try {
                // Should be bigger in production
                Thread.sleep(500)
            } catch {
                case _: InterruptedException =>
            }
        }
    })

    /**
      * Setup database, run the validation thread
      */
    def start(): Unit = {
        sql"""
drop table if exists passages
        """.execute().apply()

        sql"""
create table passages (
    id int8 not null,
    counter int not null,
    time timestamp not null,
    primary key (id, counter, time)
)
        """.execute().apply()

        receiverThread.start()
    }

    /**
      * Stop validation logger thread, new entries wont be processed
      */
    def stop(): Unit = {
        receiverThread.interrupt()
    }

    /**
      * Queue of passages awaiting processing to database
      */
    private val passagesQueue = new java.util.concurrent.LinkedBlockingQueue[LogEntry]()

    /**
      * Set of blacklisted UIDs
      */
    private val blacklist = scala.collection.parallel.mutable.ParHashSet[Long]()

    /**
      * Used to fake nano seconds.
      */
    private val n: AtomicInteger = new AtomicInteger(0)

    /**
      * Asynchronously register a log entry to the database
      * @param entry entry
      * @return true if added to queue succesfully, false otherwise
      */
    def addPassage(entry: LogEntry): Boolean = {
        val e = entry.copy(time = entry.time.withNano(n.getAndIncrement()))
        passagesQueue.add(e)
    }

    /**
      * Checks for blacklist on specific UID
      * @param uid uid
      * @return true if blacklisted, false otherwise
      */
    def isBlacklisted(uid: Long): Boolean = {
        blacklist.contains(uid)
    }

    /**
      * Manually blacklist specific UID
      * @param uid uid
      */
    def blackList(uid: Long): Unit = {
        blacklist += uid
    }
}

