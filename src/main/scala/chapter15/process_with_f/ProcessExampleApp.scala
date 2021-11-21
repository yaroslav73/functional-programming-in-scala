package chapter15.process_with_f

import chapter13._02_io_without_so.IO

object ProcessExampleApp extends App {
  import Process._

  import java.io.{BufferedReader, FileReader}

  val p: Process[IO, String] =
    await(IO(new BufferedReader(new FileReader("src/main/resources/Fahrenheit.txt")))) {
      case Right(b) =>
        lazy val next: Process[IO,String] = await(IO(b.readLine)) {
          case Left(e) => await(IO(b.close()))(_ => Halt(e))
          case Right(line) => Emit(line, next)
        }
        next
      case Left(e) => Halt(e)
    }

  println(IO.run(runLog(p)))
}
