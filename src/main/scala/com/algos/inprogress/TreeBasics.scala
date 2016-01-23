package com.algos.inprogress

import com.algos.ds.Node

object TreeBasics {
  def main(args: Array[String]): Unit = {
    val tree =
      (Node(1) /\
        (Node(2) /\
          Node(3) \
            Node(6))
          (Node(4)))(
        Node(5))

    print(treeToString(tree))

    print(height(tree))
  }

  def treeToString[A](cur: Node[A]): String = {
    def go[A] (cur: Node[A], indent: String): String = {
      val str = cur match {
        case Node(value, Some(lhs), Some(rhs)) => value + "\n" + go(lhs, indent + "<") + "\n" + go(rhs, indent + ">")
        case Node(value, Some(lhs), None) => value + "\n" + go(lhs, indent + "<")
        case Node(value, None, Some(rhs)) => value + "\n" + go(rhs, indent + ">")
        case Node(value, None, None) => value
      }
      indent + str
    }
    go(cur, "") + "\n"
  }

  def traverse[A, B](cur: Node[A], f: A => B, fParentChild: (B, B) => B, fBranch: (B, B) => B): B = {
    def t = (node: Node[A]) => traverse(node, f, fParentChild, fBranch)

    cur match {
      case Node(value, Some(lhs), Some(rhs)) => fParentChild(f(value), fBranch(t(lhs), t(rhs)))
      case Node(value, Some(lhs), None) => fParentChild(f(value), t(lhs))
      case Node(value, None, Some(rhs)) => fParentChild(f(value), t(rhs))
      case Node(value, None, None) => f(value)
    }
  }

  def height[A](cur: Node[A]): Int = cur match {
    case Node(_, Some(lhs), Some(rhs)) => 1 + Math.max(height(lhs), height(rhs))
    case Node(_, Some(lhs), None) => 1 + height(lhs)
    case Node(_, None, Some(rhs)) => 1 + height(rhs)
    case Node(_, None, None) => 1
  }

  def preorder[A]
}
