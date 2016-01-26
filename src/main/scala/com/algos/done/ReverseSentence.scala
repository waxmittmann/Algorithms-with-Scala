package com.algos.done

object ReverseSentence {
  def main(args: Array[String]): Unit = {
    println(reverse("I am a master of some forces"))
  }

  def reverse(sentence: String): String = {
    val reversed = sentence.reverse.toCharArray
    def reverseWord(start: Int, end: Int) = {
      for (i <- 0 to (end - start) / 2) {
        val t = reversed(start + i)
        reversed(start + i) = reversed(end - i)
        reversed(end - i) = t
      }
    }

    var wordStartAt = 0
    var at = 1
    while (at < reversed.length) {
      if (reversed(at) == ' ') {
        reverseWord(wordStartAt, at-1)
        wordStartAt = at + 1
      }
      at += 1
    }
    reversed.mkString("")
  }
}
