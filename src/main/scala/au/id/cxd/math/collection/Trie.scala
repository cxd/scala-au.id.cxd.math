package au.id.cxd.math.collection

/**
  * Created by cd on 24/04/2016.
  */

class Node[T](val data: T, val children: Trie[T]) {

}

object Node {
  def apply[T](data: T, children: Trie[T]) = new Node[T](data, children)
}

class Trie[T](val nodes: Seq[Node[T]]) {

  def fold[S](seed: S)(blockFn: (S, Node[T]) => S): S = {
    val state = nodes.foldLeft(seed) { (accum, node) => {
      val seed1 = accum
      val seed2 = blockFn(seed1, node)
      val seed3 = node.children.fold(seed2)(blockFn)
      seed3
    }
    }
    state
  }

}

object Trie {
  def apply[T](nodes: Seq[Node[T]]) = new Trie[T](nodes)
}
