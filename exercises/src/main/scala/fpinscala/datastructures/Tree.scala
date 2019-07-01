package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeByFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
  def maximumByFold(tree: Tree[Int]): Int = fold(tree)(v => v)(_ max _)
  def depthByFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((b1, b2) => 1 + (b1 max b2))
  def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(v => Leaf(f(v)): Tree[B])((b1, b2) => Branch(b1, b2))
}