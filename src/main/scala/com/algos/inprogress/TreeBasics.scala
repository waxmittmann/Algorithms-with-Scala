package com.algos.inprogress

import com.algos.ds.Node

object TreeBasics {
  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3) ::: List(4, 5, 6) ::: List(7, 8, 9))
    println(List(1, 2, 3) ::: (List(4, 5, 6) ::: List(7, 8, 9)))
    println((List(1, 2, 3) ::: List(4, 5, 6)) ::: List(7, 8, 9))


    val tree =
      (Node(1) /\
        (Node(2) /\
          Node(3) \
            Node(6))
          (Node(4)))(
        Node(5))
    //3, 6, 4, 1, 5

    println(treeToString(tree))
    println(treeToString2(tree))

    println(height(tree) + ", " + height(tree))

    println(preorder(tree))
    println(inorder(tree))
    println(postorder(tree))
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

//  def traverseWithState[A, B]()

  def treeToString2[A](cur: Node[A]): String = {
    val initialState = (cur, "")
    val addPrefixToValue: (A, String) => String = (a: A, prefix: String) => prefix + a
    val mergeValueAndSubtree: (String, String) => String = (cur: String, subTree: String) => s"$cur\n$subTree\n"
    val postfix = (prefix: String) => (str: String) => (prefix + str)
    val combineValueWithSubtrees = (cur: String, lhs: Option[String], rhs: Option[String]) => {
      s"$cur\n${lhs.getOrElse("s")}${rhs.getOrElse("")}"
    }

    traverseWithState2[A, String, String](
      initialState,
      addPrefixToValue,
      mergeValueAndSubtree,
      postfix("<"),  postfix(">"),
      combineValueWithSubtrees
    )
  }

  def traverseWithState2[A, B, S](cur: (Node[A], S), fV: (A, S) => B, fMerge: (B, B) => B,
                                 fLhsState: S => S, fRhsState: S => S,
                                 f: (B, Option[B], Option[B]) => (B)): (B) = {
    val nodeToVal: (Node[A], S) => B = (node: Node[A], state: S) => fV(node.value, state)

    traverseWithState(cur, fV, nodeToVal, fLhsState, fMerge, nodeToVal, fRhsState, fMerge, f)
  }


  def traverseWithState[A, B, S](cur: (Node[A], S), fV: (A, S) => B,
                                 fLhs: (Node[A], S) => (B), fLhsState: S => S, fLhsMerge: (B, B) => B,
                                 fRhs: (Node[A], S) => (B), fRhsState: S => S, fRhsMerge: (B, B) => B,
                                 f: (B, Option[B], Option[B]) => (B)): (B) = {
    def t(cur: (Node[A], S)): B = {
      traverseWithState(cur, fV, fLhs, fLhsState, fLhsMerge, fRhs, fRhsState, fRhsMerge, f)
    }

    val (curNode, curState) = cur

    val lhsResult = curNode.lhs.flatMap(lhsNode => {
      val newLhsState = fLhsState(curState)
      val lhsB = fLhs(lhsNode, curState)
      val lhsChildB =  t((lhsNode, newLhsState))
      Some(fLhsMerge(lhsB, lhsChildB))
    })

    val rhsResult = curNode.rhs.flatMap(rhsNode => {
      val newRhsState = fRhsState(curState)
      val rhsB = fRhs(rhsNode, curState)
      val rhsChildB =  t((rhsNode, newRhsState))
      Some(fRhsMerge(rhsB, rhsChildB))
    })

    val vString = fV(curNode.value, curState)

    f(vString, lhsResult, rhsResult)
  }

  def traverse[A, B](cur: Node[A], fab: A => B, f: (B, Option[B], Option[B]) => B): B = {
    cur match {
      case Node(value, Some(lhs), Some(rhs)) => f(fab(value), Some(traverse(lhs, fab, f)), Some(traverse(rhs, fab, f)))
      case Node(value, Some(lhs), None) => f(fab(value), Some(traverse(lhs, fab, f)), None)
      case Node(value, None, Some(rhs)) => f(fab(value), None, Some(traverse(rhs, fab, f)))
      case Node(value, None, None) => fab(value)
    }
  }

  def traverse2[A, B](cur: Node[A], f: A => B, fParentChild: (B, B) => B, fBranch: (B, B) => B): B = {
    traverse(cur, f, (v: B, lhsO: Option[B], rhsO: Option[B]) => {
      lhsO.fold(
        rhsO.fold(v)((rhs: B) => fParentChild(v, rhs))
      )((lhs: B) =>
        rhsO.fold(fParentChild(v, lhs))((rhs: B) => fParentChild(v, rhs))
      )
    })
  }

  def height[A](cur: Node[A]): Int =
    traverse2[A, Int](cur, _ => 1, _ + _, (a, b) => Math.max(a, b))

  def inorder[A](cur: Node[A]): List[A] = {
    traverse[A, List[A]](cur, i => List(i), (p, lhsO, rhsO) => {
      val lhs: List[A] = lhsO.getOrElse(Nil)
      val rhs: List[A] = rhsO.getOrElse(Nil)
      val result: List[A] = lhs ::: p ::: rhs
      result
    })
  }

  //1, 2, 3, 6, 4, 5
  def preorder[A](cur: Node[A]): List[A] = {
    traverse[A, List[A]](cur, i => List(i), (p, lhsO, rhsO) => {
      val lhs: List[A] = lhsO.getOrElse(Nil)
      val rhs: List[A] = rhsO.getOrElse(Nil)
      val result: List[A] = p ::: lhs ::: rhs
      result
    })
  }

  //6, 3, 4, 2, 5, 1
  def postorder[A](cur: Node[A]): List[A] = {
    traverse[A, List[A]](cur, i => List(i), (p, lhsO, rhsO) => {
      val lhs: List[A] = lhsO.getOrElse(Nil)
      val rhs: List[A] = rhsO.getOrElse(Nil)
      val result: List[A] = lhs ::: rhs ::: p
      result
    })
  }
}
