package skiticket

import java.util

import scalikejdbc._

import scala.collection.JavaConverters._

case class LogEntry(uid: Long, counter: Int)

object ValidationLogger {
    private val receiverThread: Thread = new Thread(() => {
        implicit val session = AutoSession

        val items = new util.ArrayList[LogEntry]()
        while (!Thread.currentThread().isInterrupted) {
            passagesQueue.drainTo(items)

            val queryParams = items.asScala.map(x => Seq(x.uid, x.counter))

            sql"insert into passages (id, counter) values (?, ?)"
                    .batch(queryParams).apply()

            val blackListed =
                sql"""
select uid from passages p1
 join passages p2 on p1.id = p2.id and p1.count <= p2.count
 where p1.time > p2.time
            """.map(rs => rs.long(1)).list.apply

            blacklist ++= blackListed

            items.clear()
            Thread.sleep(5000)
        }
    })

    def start(): Unit = {
        Class.forName("org.h2.Driver")
        ConnectionPool.singleton("jdbc:h2:mem:hello", "user", "pass")

        implicit val session = AutoSession

        sql"""
create table passages (
    id unsigned int8 not null primary key,
    counter int not null primary key,
    time timestamp default current_timestamp not null primary key
)
        """.execute().apply()

        receiverThread.start()
    }

    def stop(): Unit = {
        receiverThread.interrupt()
    }

    private val passagesQueue = new java.util.concurrent.LinkedBlockingQueue[LogEntry]()

    private val blacklist = scala.collection.parallel.mutable.ParHashSet[Long]()

    def addPassage(entry: LogEntry) = {
        passagesQueue.add(entry)
    }

    def isBlacklisted(uid: Long): Boolean = {
        blacklist.contains(uid)
    }
}

