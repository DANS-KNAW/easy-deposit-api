package nl.knaw.dans.easy.deposit.executor

import java.util.concurrent._

/*
 * based on insights from:
 *   - https://dzone.com/articles/scalable-java-thread-pool-executor (see workaround #2)
 *   - https://gist.github.com/mnadeem/5d01282ea4f86201ea407065e9d53cf3
 * 
 * this class has been copied from narcis-pid-graph and slightly modified on newer insights
 */
class QueuedThreadPoolExecutor(corePoolSize: Int,
                               maximumPoolSize: Int,
                               keepAliveTime: Long,
                               unit: TimeUnit,
                               workQueue: BlockingQueue[Runnable],
                               handler: RejectedExecutionHandler,
                              ) extends ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler) {
  override def setRejectedExecutionHandler(handler: RejectedExecutionHandler): Unit = {
    throw new UnsupportedOperationException("Can't set rejection handler")
  }
}
object QueuedThreadPoolExecutor {
  def apply(config: ThreadPoolConfig): QueuedThreadPoolExecutor = {
    new QueuedThreadPoolExecutor(
      corePoolSize = config.corePoolSize,
      maximumPoolSize = config.maxPoolSize,
      keepAliveTime = config.keepAliveTime,
      unit = config.unit,
      workQueue = new DynamicBlockingQueue(),
      handler = new ForceQueuePolicy(),
    )
  }
}

private class DynamicBlockingQueue[T]() extends LinkedTransferQueue[T] {
  override def add(t: T): Boolean = {
    if (super.add(t))
      true
    else { // Not possible in our case
      throw new IllegalStateException("Queue full")
    }
  }

  override def offer(t: T): Boolean = tryTransfer(t)

  override def offer(t: T, timeout: Long, unit: TimeUnit): Boolean = tryTransfer(t, timeout, unit)
}

private class ForceQueuePolicy extends RejectedExecutionHandler {
  override def rejectedExecution(r: Runnable, executor: ThreadPoolExecutor): Unit = {
    try {
      // Rejected work add to Queue.
      executor.getQueue.put(r)
    }
    catch {
      case e: InterruptedException =>
        // should never happen since we never wait
        throw new RejectedExecutionException(e)
    }
  }
}
