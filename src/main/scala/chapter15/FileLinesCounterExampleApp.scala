package chapter15

import chapter13._02_io_without_so.IO

object FileLinesCounterExampleApp extends App {
  def linesGt40k(filename: String): IO[Boolean] =
    IO {
      val _40K = 40000
      val src = io.Source.fromFile(filename) // reading from external sources like files.
      try {
        var count = 0
        val lines = src.getLines() // Obtain a stateful Iterator from the Source.
        while (count <= _40K && lines.hasNext) {
          lines.next // Has the side effect of advancing to the next element.
          count += 1
        }
        count > _40K
      } finally src.close()
    }
}
