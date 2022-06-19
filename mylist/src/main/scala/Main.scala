import scala.annotation.tailrec
import scala.collection.mutable

import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  def foldLeft[B](x: B)(f: (B, A) => B): B = {
    def inner(xs: List[A], acc: B): B = xs match {
      case Nil          => acc
      case Cons(xh, xt) => inner(xt, f(acc, xh))
    }
    inner(this, x)
  }

  def foldRight[B](x: B)(f: (A, B) => B): B = {
    this match {
      case Nil          => x
      case Cons(xh, xt) => f(xh, xt.foldRight(x)(f))
    }
  }

  def isEmpty: Boolean =
    this match {
      case Nil        => true
      case Cons(_,_ ) => false
    }

  override def toString: String = {
    def go(sb: StringBuilder, xs: List[A]): String = xs match
      case Nil => sb.append("]").result
      case Cons(xh, xt) =>
        go(
          sb.append(xh)
            .append(if xt == Nil then "" else ", "),
          xt
        )
    go(new StringBuilder("["), this)
  }

object List:
  def empty[A]: List[A] = Nil

  def apply[A](xs: A*): List[A] = of(xs*)

  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) =>
      Cons(x, acc)
    }

def sum(l: List[Int]): Int = l.foldLeft(0)(_ + _)

def dotProduct(u: List[Int], v: List[Int]): Int =
  zipWith(u, v, (x, y) => x * y).foldLeft(0)(_ + _)

def zipWith[A, B, C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] =
  (xs, ys) match {
    case (List.Nil, _) | (_, List.Nil) => List.Nil
    case (List.Cons(xh, xt), List.Cons(yh, yt)) =>
      List.Cons(f(xh, yh), zipWith(xt, yt, f))
  }

@main def main(): Unit = {

println("Hello World!")

}
