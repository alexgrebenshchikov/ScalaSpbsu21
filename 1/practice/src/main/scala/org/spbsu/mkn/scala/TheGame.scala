package org.spbsu.mkn.scala

import scala.::
import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException


  def generateNumberString(length: Int, possibleSymbols: IndexedSeq[Char] = ('0' to '9') ++ ('A' to 'Z'),
                           random: Random = new Random()): String = {
    length match {
      case 0 => ""
      case n =>
        val reply = possibleSymbols(random.nextInt(possibleSymbols.length))
        reply + generateNumberString(n - 1, possibleSymbols.filter(_ != reply), random)
    }
  }

  def checkRepeatedDigits(input: String): Boolean = input.toSet.size == input.length

  def countBulls(secret: String, userInput: String): Int = {
    var bulls = 0
    for (i <- 0 until secret.length) {
      if (secret(i) == userInput(i))
        bulls += 1
    }
    bulls
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (!checkRepeatedDigits(secret))
      throw new RepeatingDigitsException()
    val bulls = countBulls(secret, userInput)
    if (bulls == secret.length)
      return Correct(numTries)
    val cows = secret.toSet.intersect(userInput.toSet).size - bulls
    Incorrect(bulls, cows)
  }

  @tailrec
  def askGuess(secret: String, name: String, counterTries: Int = 1): Unit = {
    println(s" Try #$counterTries")
    val guess = readLine()
    try {
      val res = validate(secret, guess, counterTries)
      res match {
        case correct: Correct =>
          println(s"You're right, $name! Number of tries is ${correct.numTries}")
          return
        case incorrect: Incorrect =>
          println(s"Number of bulls : ${incorrect.bulls}")
          println(s"Number of cows : ${incorrect.cows}")
      }
    } catch {
      case _: RepeatingDigitsException =>
        println("Repeated digits found!")
      case _: WrongNumberLengthException =>
        println("Wrong length!")
    }
    askGuess(secret, name, counterTries + 1)
  }

  @tailrec
  def askLength(): Int = {
    try {
      readLine().toInt
    } catch {
      case _: Throwable => println("Incorrect input!")
        askLength()
    }
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")
    println("Enter the length of secret.")
    val len = askLength()
    val secret = generateNumberString(len)
    println("I thought of a secret! Try to guess it!")
    askGuess(secret, name)
  }
}
