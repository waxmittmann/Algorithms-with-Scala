package com.algos.done

import com.algos.done.Points.Points
import com.algos.done.Suite.Suite

import scala.util.Random


object Points extends Enumeration {
  type Points = Value
  val _7, _8, _9, _10, J, Q, K, A = Value
}

object Suite extends Enumeration {
  type Suite = Value
  val Spade, Diamond, Heart, Club = Value
}

case class Card(points: Points, suite: Suite) {
  override def toString(): String = {
    s"[$points, $suite]"
  }
}

object Deck {
  def create: Deck = {
    val deckCards: List[Card] = Points.values.foldLeft(List[Card]())((deck: List[Card], curValue: Points) => {
      val suiteCards: List[Card] = Suite.values.toList.map((curSuite: Suite) => Card(curValue, curSuite))
      suiteCards ::: deck
    })
    Deck(deckCards)
  }
}

case class Deck(cards: List[Card]) {
  def shuffle(): Deck = {
    Deck(Random.shuffle(cards))
  }

  def drawN(nr: Int): Option[(List[Card], Deck)] = {
    if (cards.size >= nr) {
      Some(cards.take(nr), Deck(cards.drop(nr)))
    } else {
      None
    }
  }

  def draw(): Option[(Card, Deck)] = {
    if (cards.isEmpty)
      None
    else
      Some(cards.head, Deck(cards.tail))
  }

  override def toString(): String = {
    cards.mkString(", ")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val unshuffledDeck = Deck.create
    println(unshuffledDeck.cards.take(6))

    val shuffledDeck = unshuffledDeck.shuffle()
    println(shuffledDeck.cards.take(6))

    for {
      result1 <- shuffledDeck.draw()
      result2 <- result1._2.draw()
      result3 <- result2._2.draw()
      result4 <- result3._2.draw()
    } yield {
      println(s"${result1._1}, ${result2._1}, ${result3._1}, ${result4._1}")
    }
    println(shuffledDeck.drawN(4).get._1)
  }
}
