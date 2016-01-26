package com.algos.inprogress

object MaxDifference {
  def main(args: Array[String]): Unit = {
    println(maxDiff(Array(3, 5, 11, 1, 3, 1, 7, 0, 5)))
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

}
