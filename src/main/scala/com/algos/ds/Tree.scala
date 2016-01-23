package com.algos.ds


sealed trait Tree

object Tree {
  def apply[A](value: A) = Node(value, None, None)
}

case class Node[A](value: A, lhs: Option[Node[A]] = None, rhs: Option[Node[A]] = None) {
  def /\ (lhs: Node[A])(rhs: Node[A]): Node[A] = {
    Node(value, Some(lhs), Some(rhs))
  }

  def / (lhs: Node[A]): Node[A] = {
    Node[A](value, Some(lhs), None)
  }

  def \ (rhs: Node[A]): Node[A] = {
    Node[A](value, None, Some(rhs))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    Node(1)./\(
      Node(2)./\(
        Node(3))(Node(4)))(
      Node(5))

    val tree =
      (Node(1) /\
        (Node(2) /\
          Node(3) \
            Node(6))
          (Node(4)))(
        Node(5))

//    print(treeToString(tree))

  }
}
