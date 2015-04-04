
object polynomials {

  class Poly(terms0: Map[Int, Double]) {

    /**
     * Auxiliary Constructor using exponent/Coefficient pais
     * @param bindings
     * @return
     */
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    /* Set default value of 0.0 to represent all the powers of
    * x with a zero coefficient. I.e., the missing key-values in
    * the Map terms*/
    val terms = terms0 withDefaultValue 0.0

    /**
     * Adds the terms of this Polynomial with the terms of the other Polynomial
     * TODO: Implement with foldLeft
     * @param other
     * @return
     */
    def + (other: Poly): Poly = {
      new Poly(other.terms.foldLeft(terms)( addTerms(_,_)))
    }

    def addTerms(terms: Map[Int, Double], term: (Int, Double)):Map[Int, Double] = {
      val (exp, coeff) = term
      val newCoeff = coeff + terms(exp)
      terms.updated(exp, newCoeff)
    }

    /**
     *
     * @param term
     * @return
     */
    def adjust(term: (Int, Double)):(Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString = {
      ( for( (exp, coeff) <- terms.toList.sorted.reverse) yield (coeff + "x^" + exp) ) mkString " + "
    }

  }

  val p1 = new Poly(0 -> 5, 1 -> -1, 2 -> 1)
  val p2 = new Poly(0 -> -5, 1 -> 1, 2 -> -1, 3 -> 1)

  p1 + p2

}