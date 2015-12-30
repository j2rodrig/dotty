class Dotmod1 {
  def method(): Dotmod1 = { this.x = 10; this; }
  val r = method()
  var x = 2
}
