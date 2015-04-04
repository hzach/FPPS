def expr = {
  val x = {print("x"); 1}
  lazy val y = {print("y"); 2}
  val z = {print("z"); 3}
  z + y + x + z + y + x
}

expr