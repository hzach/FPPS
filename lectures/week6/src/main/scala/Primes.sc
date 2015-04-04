
def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  val pairs = for {xy <- xs zip ys} yield (xy._1 * xy._2)
  pairs.foldLeft(0.0)(_+_)
}

scalarProduct(List(1,2,3), List(2,3,4))
