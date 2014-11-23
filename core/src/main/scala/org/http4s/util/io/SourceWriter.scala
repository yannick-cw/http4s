package org.http4s.util.io

import java.io.Writer

import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.async.{boundedQueue, unboundedQueue}
import scalaz.stream.async.mutable.Queue

/**
 * Adapts a Writer to a source of Strings.  The Writer is backed by a queue,
 * and may block if the queue is full.  Each write results in one output
 * in the source.  It is recommended to wrap this in a BufferedWriter.
 *
 * To avoid deadlock, the source should be run on a different thread than
 * the writes.
 */
class SourceWriter private (queue: Queue[String]) extends Writer {
  def source: Process[Task, String] = queue.dequeue

  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    val str = new String(cbuf, off, len)
    queue.enqueueOne(str).run
  }

  override def flush(): Unit = {}

  override def close(): Unit = queue.close.run
}

object SourceWriter {
  /**
   * Creates a SourceWriter.
   *
   * @param queueSize The size of the bounded queue backing the writer.
   *                  Unless positive, the queue is unbounded.
   */
  def apply(queueSize: Int = 0) = {
    val queue =
      if (queueSize > 0) boundedQueue[String](queueSize)
      else unboundedQueue[String]
    new SourceWriter(queue)
  }
}
