package chapter08

trait Prop {
  def check(): Boolean
  def &&(p: Prop): Prop = () => this.check() && p.check()
}
