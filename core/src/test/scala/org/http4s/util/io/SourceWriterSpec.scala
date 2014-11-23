package org.http4s.util.io

import org.http4s.Http4sSpec

import scalaz.concurrent.Task
import scalaz.Nondeterminism

class SourceWriterSpec extends Http4sSpec {
  "emit all written elements" in {
    prop { (strings: Seq[String], queueSize: Int) =>
      val writer = SourceWriter(queueSize)
      val writeTask = Task {
        for (string <- strings) writer.write(string)
        writer.close()
        Vector.empty[String]
      }
      val logTask = Task.fork { writer.source.runLog }.map(_.toVector)
      val Seq(_, log) = Nondeterminism[Task].gather(Seq(writeTask, logTask)).run
      log must_== strings
    }
  }

  "be non-blocking when queue size is 0" in {
    prop { (strings: Seq[String]) =>
      val writer = SourceWriter(0)
      for (string <- strings) writer.write(string)
      writer.close()
      val log = writer.source.runLog.run
      log must_== strings
    }
  }
}
