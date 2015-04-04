/**
 * Created by zach on 3/9/15.
 */
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(s: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def union(s: IntSet): IntSet = s

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else
      if (x > elem) new NonEmpty(elem, left, right incl x)
      else this


  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else
      if (x > elem) right contains x
      else true

  override def union(s: IntSet): IntSet = ((left union right) union s) incl elem

  override def toString = "{" + left + elem + right + "}"

}
