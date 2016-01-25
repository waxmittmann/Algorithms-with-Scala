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

    println(height(tree) + ", " + height(tree))

    println(preorder(tree))
    println(inorder(tree))
    println(postorder(tree))
  }

  /** *
    *  The base stuff
    */
  def traverseWithState[A, B, S](cur: (Node[A], S), fV: (A, S) => B, fLhsState: S => S, fRhsState: S => S,
                                 f: (B, Option[B], Option[B]) => (B)): (B) = {
    def traverse(cur: (Node[A], S)): B = {
      traverseWithState(cur, fV, fLhsState, fRhsState, f)
    }

    val (curNode, curState) = cur
    def processNode(stateTransform: S => S): (Node[A]) => Option[B] = {
      (curNode => {
        val newState = stateTransform(curState)
        val result =  traverse((curNode, newState))
        Some(result)
      })
    }

    val vString = fV(curNode.value, curState)

    val lhsResult = curNode.lhs.flatMap(processNode(fLhsState)(_))
    val rhsResult = curNode.rhs.flatMap(processNode(fRhsState)(_))

    f(vString, lhsResult, rhsResult)
  }

  def traverseNode2[A, B](cur: Node[A], fab: Node[A] => Option[B], f: (Option[B], Option[B], Option[B]) => Option[B]): Option[B] = {
    cur match {
      case n @ Node(value, Some(lhs), Some(rhs)) => f(fab(n), traverseNode2(lhs, fab, f), traverseNode2(rhs, fab, f))
      case n @ Node(value, Some(lhs), None) => f(fab(n), traverseNode2(lhs, fab, f), None)
      case n @ Node(value, None, Some(rhs)) => f(fab(n), None, traverseNode2(rhs, fab, f))
      case n @ Node(value, None, None) => fab(n)
    }
  }

  def traverseNode[A, B](cur: Node[A], fab: Node[A] => B, f: (B, Option[B], Option[B]) => B): B = {
    traverseNode2(cur, (n: Node[A]) => Some(fab(n)), (v: Option[B], lhs: Option[B], rhs: Option[B]) => {
      Some(f(v.get, lhs, rhs))
    }).get
  }

  def traverse[A, B](cur: Node[A], fab: A => B, f: (B, Option[B], Option[B]) => B): B = {
    traverseNode[A, B](cur, n => fab(n.value), f)
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

  /** *
    * Applications
    */
//  def traverseWithState[A, B, S](cur: (Node[A], S), fV: (A, S) => B, fLhsState: S => S, fRhsState: S => S,
//                                 f: (B, Option[B], Option[B]) => (B)): (B) = {

  /*def lowestCommonAncestor[A](root: Node[A], valueA: A, valueB: A): Node[A] = {
    val initial: (Node[A], Option[Node[A]]) = (root, None)

    val fv = (cur: Node[A], s: Option[Node[A]]) => {
      s.fold({
        if (cur.value == valueA || cur.value == valueB)
          Some(cur)
        else
          None
      })(foundPreviously => {
        //Already found it
        if (foundPreviously.value != valueA && foundPreviously.value != valueB) {
          Some(foundPreviously)
        } else if ((foundPreviously.value == valueA && cur.value == valueB) || (foundPreviously.value == valueB && cur.value == valueA)) {
          Some(cur)
        } else {
          Some(foundPreviously)
        }
      })
    }

    traverseWithState[A, Node[A], Node[A]](initial, fv, identity, identity, )
  }*/

  case class Result[A](rType: String, node: Node[A])


  /*
  def lowestCommonAncestor[A](root: Node[A], valueA: A, valueB: A): Node[A] = {
    traverseNode2[A, Result[A]](root,
      (n: Node[A]) => {
        if (n.value == valueA) Some(Result[A]("a", n))
        else if(n.value == valueB) Some(Result[A]("b", n))
        else None
      },
      (c, lhs, rhs) => {
        if (c.r)
      })

  }*/


/*
  def lowestCommonAncestor[A](root: Node[A], valueA: A, valueB: A): Node[A] = {
    //  def traverse[A, B](cur: Node[A], fab: A => B, f: (B, Option[B], Option[B]) => B): B = {
    traverseNode2[A, Node[A]](root, (n: Node[A]) => if (n.value == valueA || n.value == valueB) Some(n) else None,

      (n: Node[A]) => if (n.value == valueA || n.value == valueB) Some(n) else None,
      (curNodeO, leftNodeO, rightNodeO) => {
        leftNodeO.fold({
          rightNodeO.fold({
            curNodeO
          })(rightNode => {
            if (rightNode.value != valueA && rightNode.value != valueB) {
              Some(rightNode)
            }

            curNodeO.fold({
              Some(rightNode)
            })(curNode => {
              if ((rightNode.value == valueA && curNode.value == valueB) || (rightNode.value == valueB && curNode.value == valueA)) {
                Some(curNode)
              } else {
                Some(rightNode)
              }
            })
          })
        })(leftNode => {
          if (leftNode.value != valueA && leftNode.value != valueB) {
            Some(leftNode)
          }

          curNodeO.fold({
            Some(leftNode)
          })(curNode => {
            if ((leftNode.value == valueA && curNode.value == valueB) || (leftNode.value == valueB && curNode.value == valueA)) {
              Some(curNode)
            } else {
              Some(leftNode)
            }
          })
        })
      })
  }*/

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

  def treeToString[A](cur: Node[A]): String = {
    val initialState = (cur, "")
    val addPrefixToValue: (A, String) => String = (a: A, prefix: String) => prefix + a
    val postfix = (postfix: String) => (str: String) => (str + postfix)
    val combineValueWithSubtrees = (cur: String, lhs: Option[String], rhs: Option[String]) => {
      s"$cur\n${lhs.getOrElse("")}${rhs.getOrElse("")}"
    }

    traverseWithState[A, String, String](
      initialState,
      addPrefixToValue,
      postfix("<"),  postfix(">"),
      combineValueWithSubtrees
    )
  }
}
