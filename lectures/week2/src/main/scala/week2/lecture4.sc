class Rational(a:Int, b:Int) {

  require(b != 0, "denominator must be nonzero")

  private def gcd(a:Int, b:Int):Int = if ( b == 0 ) a else gcd(b, a%b)
  private val g = gcd(a, b)

  def numer = a
  def denom = b

  def +(that: Rational) = new Rational(a*that.denom + that.numer*b, b*that.denom)

  def unary_- :Rational = new Rational(-a, b)

  def -(that: Rational) = this + -that

  def <(that: Rational) = numer*that.denom < that.numer*denom

  override
  def toString() = {
    a/gcd(a, b) + "/" + b/gcd(a, b)
  }

}

val x = new Rational(1028, 3)
val y = new Rational(12387998, 7)
val z = new Rational(3, 2)

x<y

x-y-z+x-y+z