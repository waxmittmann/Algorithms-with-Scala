package com.algos.inprogress

object MaxDifference {
  def main(args: Array[String]): Unit = {
    //println(maxDiff(Array(3, 5, 11, 1, 3, 1, 7, 0, 5)))
    maxDiffK(Array(3, 4, 1, 3, 2, 5), 3)
  }

  //3, 2, 5, 4, 8,
  def maxDiff(arr: Array[Int]): Int = {
    var minSoFarAt = 0
    var maxDiff = 0
    var maxDiffAt = 0

    for (i <- 1 until arr.length) {
      val at = arr(i)
      if (at < arr(minSoFarAt)) {
        minSoFarAt = i
      } else if ((at - arr(minSoFarAt)) > maxDiff) {
        maxDiff = at - arr(minSoFarAt)
        maxDiffAt = i
      }
    }
    maxDiff
  }

  def maxDiffK(arr: Array[Int], k: Int): Int = {
    val diffs: Array[Array[Int]] = Array.ofDim[Int](arr.length, arr.length)

    for (i <- 0 until arr.length) {
      var curMin = arr(i)
      var curMaxDiff = 0
      for (j <- i until arr.length) {
        if (curMin > arr(j)) {
          curMin = arr(j)
        } else if (arr(j) - curMin > curMaxDiff) {
          curMaxDiff = arr(j) - curMin
        }
        diffs(i)(j) = curMaxDiff
      }
    }

    var bestAtN = diffs(0)
    println(bestAtN.toList)
    for (i <- 1 until k) {
      val newBestAtN = Array.fill(arr.length)(0)

      for (j <- (i + 1) until arr.length) {
        val curDiffsSum = bestAtN(j-1) + diffs(j)(arr.length-1) //Hmm, probably only need to store the prefix sum array, instead of 2d

        if (curDiffsSum > newBestAtN(j - 1)) {
          newBestAtN(j) = curDiffsSum
        } else {
          newBestAtN(j) = newBestAtN(j - 1)
        }
      }

      bestAtN = newBestAtN
      println(bestAtN.toList)
    }

    println(diffs.map(_.toList + "\n").toList)
    println(bestAtN.toList)
    0
  }


}
