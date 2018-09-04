import scala.collection.TraversableLike

package object tinybasic {

  implicit class TraversableOps[X, Repr <: TraversableLike[X, Repr]](xs: TraversableLike[X, Repr]) {

    def single(pred: (X) => Boolean): X = {
      val found = xs.filter(pred)
      require(found.size == 1, s"Expected single value, got ${found.size}")
      found.head
    }
  }
}
