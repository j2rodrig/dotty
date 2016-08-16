trait A { type T; type M >: T }
trait B extends A {
  val x : String;
  val u : A { type T = B.this.T } ;
  type T = String //x.type;  // !!!! errors when x.type is substituted for String
  //type M = u.M  // !!!! errors when this line is enabled
}
