/*
 * Copyright (C) 2018 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
