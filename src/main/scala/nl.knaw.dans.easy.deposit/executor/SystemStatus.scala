/**
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

import java.util.concurrent.ThreadPoolExecutor

import org.json4s.{ DefaultFormats, Formats }

case class ThreadPoolStatus(poolSize: Int,
                            activeThreads: Int,
                            largestPoolSize: Int,
                            taskCount: Long,
                            completedTaskCount: Long,
                           )
object ThreadPoolStatus {
  def from(threadPoolExecutor: ThreadPoolExecutor): ThreadPoolStatus = {
    ThreadPoolStatus(
      poolSize = threadPoolExecutor.getCorePoolSize,
      activeThreads = threadPoolExecutor.getActiveCount,
      largestPoolSize = threadPoolExecutor.getLargestPoolSize,
      taskCount = threadPoolExecutor.getTaskCount,
      completedTaskCount = threadPoolExecutor.getCompletedTaskCount,
    )
  }
}

case class SystemStatus(threadPoolStatus: ThreadPoolStatus,
                        queueSize: Int,
                        queueContent: Seq[String],
                       )
object SystemStatus {
  implicit val jsonFormatter: Formats = new DefaultFormats {}
}
