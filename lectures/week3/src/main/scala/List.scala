/**
 * Created by zach on 3/9/15.
 */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false

}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

def fn[T](list: List[T], n: Int):T = {

  if (list.isEmpty) throw new IndexOutOfBoundsException
  else
    if (n == 0) list.head
    else fn(list.tail, n - 1)

}
