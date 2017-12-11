package skiticket.data

import java.time.LocalDateTime
import java.util
import java.util.concurrent.atomic.AtomicInteger

import scalikejdbc._

import scala.collection.JavaConverters._

case class LogEntry(uid: Long, counter: Int, time: LocalDateTime = LocalDateTime.now())

object ValidationLogger {
    GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
        enabled = false,
        singleLineMode = true,
        printUnprocessedStackTrace = false
    )
    Class.forName("org.h2.Driver")
    ConnectionPool.singleton("jdbc:h2:mem:hello", "user", "pass")

    implicit val session = DB.autoCommitSession()

    def getAll = {
        sql"select * from passages".map(rs => (rs.long(1), rs.int(2), rs.dateTime(3))).list.apply
    }

    private val receiverThread: Thread = new Thread(() => {
        val items = new util.ArrayList[LogEntry]()
        while (!Thread.currentThread().isInterrupted) {
            //            println("Validation logger flush")
            passagesQueue.drainTo(items)

            val queryParams = items.asScala.map(x => Seq(x.uid, x.counter, x
                    .time.toString))

            sql"insert into passages values (?, ?, ?)"
                    .batch(queryParams: _*).apply()

            val blackListed =
                sql"""
select p1.id, p1.counter as p1c, p1.time as p1t, p2.counter as p2c, p2.time as p2t
 from passages p1
 join passages p2 on p1.id = p2.id and p1.counter <= p2.counter
 where p1.time > p2.time
            """.map(rs => rs.toMap()).list.apply

            if (blackListed.nonEmpty) {
                println(s"Found fraud, will blacklist because: $blackListed")

                blacklist ++= blackListed.map(x => x("id").toString.toLong)
            }

            items.clear()
            try {
                Thread.sleep(5000)
            } catch {
                case _: InterruptedException =>
            }
        }
    })

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

    def stop(): Unit = {
        receiverThread.interrupt()
    }

    private val passagesQueue = new java.util.concurrent.LinkedBlockingQueue[LogEntry]()

    private val blacklist = scala.collection.parallel.mutable.ParHashSet[Long]()

    private val n: AtomicInteger = new AtomicInteger(0)
    def addPassage(entry: LogEntry): Boolean = {
        val e = entry.copy(time = entry.time.withNano(n.getAndIncrement()))
        passagesQueue.add(e)
    }

    def isBlacklisted(uid: Long): Boolean = {
        blacklist.contains(uid)
    }

    def blackList(uid: Long): Unit = {
        blacklist += uid
    }
}

