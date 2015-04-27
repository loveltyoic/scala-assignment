package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("signal") {
    val num = Var(2.0)
    val twice = Signal(num() * 2)
    num() = 3.0
    assert(twice() === 6.0)
  }

  test("root") {
    val a = Var(2.0)
    val b = Var(3.0)
    val c = Var(1.0)
    val delta = Polynomial.computeDelta(a, b, c)
    val root = Polynomial.computeSolutions(a, b, c, delta)
    assert(root() === Set(-1.0, -0.5))
    a() = 1.0
    b() = -2.0
    assert(delta() === 0)
    assert(root() === Set(1))
  }

  test("calculate") {
    val a: Expr = Literal(1)
    val b: Expr = Literal(2)
    val c: Expr = Plus(Ref("a"), Ref("b"))
    val expressions = Map("a" -> Signal(a), "b" -> Signal(b), "c" -> Signal(c))
    assert(Calculator.computeValues(expressions)("c")() === 3.0)
  }

  test("cycle") {
    val a: Expr = Ref("b")
    val b: Expr = Ref("c")
    val c: Expr = Plus(Ref("a"), Literal(1))
    val nameExpressions = Map("a" -> Signal(a), "b" -> Signal(b), "c" -> Signal(c))
    assert(Calculator.computeValues(nameExpressions)("c")() equals Double.NaN)
  }
}
