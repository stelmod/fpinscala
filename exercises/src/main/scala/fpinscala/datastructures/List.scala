package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }


  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, xs) => if (f(h)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, xs) => Cons(h, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0) ((_: A, z: Int) => z + 1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, xs) => foldLeft(xs, f(z, h)) (f)
  }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r) ((a, b) => Cons(a, b))
  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r) ((b, a) => Cons(a, b))

  def sumLeft(ns: List[Int]): Int = foldLeft(ns, 0) (_ + _)
  def productLeft(ns: List[Double]): Double = foldLeft(ns, 1.0) (_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()) ((acc: List[A], z: A) => Cons(z, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, xs) => Cons(f(h), map(xs)(f))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, xs) => if (f(h)) Cons(h, filter(xs)(f)) else filter(xs)(f)
  }

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(e => if (f(e)) List(e) else Nil)

  def flatMap[A, B](as: List[A]) (f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, xs) => append(f(h), flatMap(xs)(f))
  }

  def zipInt(l1: List[Int], l2: List[Int]): List[Int] =  (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, xs1), Cons(h2, xs2)) => Cons(h1 + h2, zipInt(xs1, xs2))
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =  (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, xs1), Cons(h2, xs2)) => Cons(f(h1, h2), zipWith(xs1, xs2)(f))
  }
}

object TestList {

  import List._

  // test implementation of `fib`
  def main(args: Array[String]): Unit = {
    println("x = " + x)
    println(tail(List(1, 2, 3, 4)))
    println(setHead(List(1, 2, 3, 4), -1))
    println(drop(List(11, 12, 13, 14), 3))
    println(dropWhile(List(11, 22, 33, 44, 12), (y: Int) => y < 30))
    println(init(List(11, 12, 13, 14)))
    println(length(List(11, 12, 13, 14, 15)))

    val l = List(1, 2, 3, 5)
    val l2 = List(11, 12, 13, 15)
    val ld = List(1.0, 2.0, 3.0, 5.0)
    println(foldLeft(l, 0) (_ + _))

    println(sum2(l) + " " + product2(ld))
    println(sumLeft(l) + " " + productLeft(ld))
    println(reverse(l))
    println(appendViaFoldRight(l, l2))
    println(appendViaFoldLeft(l, l2))
    println(map(l) (_ + 1))
    println(filter(l) (_ % 2 == 0))

  }
}