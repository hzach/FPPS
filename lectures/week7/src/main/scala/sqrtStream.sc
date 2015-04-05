/* Method profiler*/
def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block    // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}
def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: ( guesses map improve)
  guesses
}
sqrtStream(2).take(6).toList.last
def from(n:Int): Stream[Int] = n#::from(n+1)
val N: Int = 5
//Question: Which is faster: Map or Filter?
time { (from(1) map( _ * N)).take(1000).toList }
time { (from(1) filter(_ % N == 0)).take(1000).toList}
/* Answer: Map, because we are not producing unnecessary stream elements. Rather
 * we are simply defining an onto function between every integer in the stream to
 * its multiple of N. Whereas with the Filter method, we have to visit each integer
 * and maybe place in the the new Stream of Multiples of N, if it matches the filter
 * predicate. */