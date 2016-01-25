package com.algos.done

/**
 * Created by maxwittman on 25/01/2016.
 */
object GCD {
  /*
          145, 21


          17, 7
          10, 7
          7, 3
          4, 3
          3, 1


          18, 12
          12, 6
          6, 6

   */

  def main(args: Array[String]): Unit = {
    println(gcd(834, 23))
    println(gcd(114, 38))
    println(gcd(923, 235))
    println(gcd(66, 235))
    println(gcd(132, 77))
    println(gcd(30, 12))
  }

//  11 * 3 * 2 * 2 = 132
//  11 * 7 = 77

  def gcd(a: Int, b: Int): Int = {
    if (b <= 0) {
      a
    } else {
      val (an, bn) = {
        if (a - b > b)
          (a-b, b)
        else
          (b, a-b)
      }
      gcd(an, bn)
    }
  }
}
