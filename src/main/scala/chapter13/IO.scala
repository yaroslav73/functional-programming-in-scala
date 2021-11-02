package chapter13

trait IO { self =>
  def run(): Unit
  def ++(io: IO): IO = () => {
    self.run()
    io.run()
  }
}

object IO {
  def empty: IO = () => ()
}
