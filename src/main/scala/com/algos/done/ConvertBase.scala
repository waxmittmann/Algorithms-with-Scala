package com.algos.done

/**
 * Created by maxwittman on 25/01/2016.
 */
object ConvertBase {
  def main(args: Array[String]): Unit = {
    println("16, 10: " + convert(16, 10, "FF"))
    println("10, 16: " + convert(10, 16, "100"))
    println("14, 7: " + convert(14, 7, "AC")) //Expect 305
    //Preeeetty sure it's working, but too lazy to manually calc =/
    println("14, 7: " + convert(14, 7, "AC2B"))
  }

  val intToChar = Map(
    (0 ->'0'),
    (1 ->'1'),
    (2 ->'2'),
    (3 ->'3'),
    (4 ->'4'),
    (5 ->'5'),
    (6 ->'6'),
    (7 ->'7'),
    (8 ->'8'),
    (9 ->'9'),
    (10 ->'A'),
    (11 ->'B'),
    (12 ->'C'),
    (13 ->'D'),
    (14 ->'E'),
    (15 ->'F'))

  val charToInt = Map(
    ('0' ->0),
    ('1' ->1),
    ('2' ->2),
    ('3' ->3),
    ('4' ->4),
    ('5' ->5),
    ('6' ->6),
    ('7' ->7),
    ('8' ->8),
    ('9' ->9),
    ('A' ->10),
    ('B' ->11),
    ('C' ->12),
    ('D' ->13),
    ('E' ->14),
    ('F' ->15))

  def convert(baseOriginal: Int, baseNew: Int, numberOriginal: String): String = {
    def convertToBase10(base: Int, number: String): Int = {
      var result = 0
      var mult = 1
      for (i <- 0 until number.length) {
        result += charToInt(number(i)) * mult
        mult *= base
      }
      result
    }

    /*
          100
          100 % 16 = 4
          100 / 16 = 6
          6 % 16 = 6
          6 / 16 = 0
     */
    def convertToBaseN(base: Int, number: Int): String = {
      var result = ""

      var numberAt = number

      while (numberAt > 0) {
        result = intToChar(numberAt % base) + result
        numberAt /= base
      }
      result
    }

    val reversedNumberOriginal = numberOriginal.reverse
    val numberInBase10 = convertToBase10(baseOriginal, reversedNumberOriginal)
    println("In base 10: " + numberInBase10)
    val newNumber = convertToBaseN(baseNew, numberInBase10)
    println("Number in base N: " + newNumber)
    newNumber
  }
}
