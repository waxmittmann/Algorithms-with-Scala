package com.algos.done

object DivWithSub {

  def main(args: Array[String]): Unit = {
    println(div(50, 5))
    println(div(49, 5))
    println(div(1023, 25))
    println(div(821373, 97))
  }

  def div(a: Int, b: Int): Int = {
    var result = 0
    var aRemainder = a
    while (aRemainder > b) {
      var factor = 1
      while ((factor*2) * b <= aRemainder) {
        factor *= 2
      }
      result += factor
      aRemainder -= factor * b
    }

    result
  }
}
