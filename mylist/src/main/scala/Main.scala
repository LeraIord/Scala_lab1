import scala.annotation.tailrec
import scala.collection.mutable
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  def tail: List[A] = this match{
    case Nil => Nil
    case Cons(hd, tl) => tl
  }

  def tryHead: Option[A] = this match {
    case Nil => None
    case Cons(hd,tl)=> Some(hd)
  }

  def map[B](f: (A) => B): List[B] = {
    def go(xs: List[A], acc: List[B]): List[B] = {
      xs match {
        case List.Nil => acc
        case List.Cons(xh, xt) => List.Cons(f(xh), go(xt, acc))
      }
    }
    go(this,List.Nil)
  }

  def foldLeft[B](x: B)(f: (B, A) => B): B = {
    @tailrec
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
      case Cons(_, _) => false
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

def sum_old(xs: List[Int]): Int = xs.foldLeft(0)(_ + _)  

def sum(xs: List[Int]): Int = xs match {
  case List.Nil => 0 
  case List.Cons(xh,xt) => xh + sum(xt)
}

def dotProduct_old(u: List[Int], v: List[Int]): Int =
  zipWith(u, v, (x, y) => x * y).foldLeft(0)(_ + _) 

def dotProduct(xs: List[Int], ys: List[Int]): Int = {
  def go(xs: List[Int], ys: List[Int], acc: Int): Int = (xs, ys) match {
    case (List.Cons(xh , xt), List.Cons(yh , yt)) => go(xt , yt, acc + xh * yh)
    case (List.Nil, _) => acc
    case (_, List.Nil) => acc
  }

  go(xs, ys, 0)
}

def zipWith[A, B, C](xs: List[A], ys: List[B], f: (A, B) => C): List[C] =
  (xs, ys) match {
    case (List.Nil, _) | (_, List.Nil) => List.Nil
    case (List.Cons(xh, xt), List.Cons(yh, yt)) =>
      List.Cons(f(xh, yh), zipWith(xt, yt, f))
  }

def map[A](list: List[A], f: (A) => A): 
  List[A] = {
    def go(xs: List[A], acc: List[A]): List[A] = {
      xs match {
        case List.Nil => acc
        case List.Cons(xh, xt) => List.Cons(f(xh), go(xt, acc))
      }
    }
    go(list,List.Nil)
  }

//Намагалася, але виникає помилка. 
// def transpose[T](l: List[List[T]]): List[List[Option[T]]] =
//    l map {_.tryHead} match {
//        case List.Nil => List.Nil
//        case List.Cons(head,tail) => List.Cons(head, transpose(l.tail))
//    }




@main def main(): Unit = {

println("Hello World!")

}


