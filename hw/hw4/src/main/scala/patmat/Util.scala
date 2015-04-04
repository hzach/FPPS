package patmat

import scala.math.Ordering
import Huffman._
object Util {

  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.size / 2
    if (n == 0) xs
    else {

      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (u :: us, v :: vs) => {
          if ( ord.lt(u, v)) u :: merge(us, ys)
          else v :: merge(xs, vs)
        }
      }

      val (fst, snd) = xs splitAt n
      merge(mergeSort(fst), mergeSort(snd))

    }
  }

  implicit def ord[T <: CodeTree] = new Ordering[T] {
    override def compare(x: T, y: T) = x.weight compare y.weight
  }
}
