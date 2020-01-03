package nl.knaw.dans.easy.deposit.executor

import java.util.concurrent.{ ThreadPoolExecutor, TimeUnit }

import scala.collection.JavaConverters._
import scala.util.Try

class JobQueueManager(threadPool: ThreadPoolExecutor) extends AutoCloseable {

  def this(config: ThreadPoolConfig) = this(QueuedThreadPoolExecutor(config))

  def scheduleJob(job: Runnable): Try[Unit] = Try { threadPool execute job }

  def getSystemStatus: SystemStatus = {
    val queue = threadPool.getQueue.asScala.toList.map(_.toString)
    SystemStatus(
      threadPoolStatus = ThreadPoolStatus.from(threadPool),
      queueSize = queue.size,
      queueContent = queue,
    )
  }

  override def close(): Unit = {
    threadPool.awaitTermination(20000, TimeUnit.MILLISECONDS)
  }
}
