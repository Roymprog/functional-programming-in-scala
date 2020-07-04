package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Methods should ideally be made tailrecursive
object Tree {
  // 3.25 Count number of nodes (leaves and branches)
  def count[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + count(l) + count(r)
  }

  // 3.26 Get max value in tree
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(l, r) => max(l) max max(r)
  }

  // 3.27 Return max depth of tree
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // 3.28 Map function for tree
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29 fold function that allows for generalizing count, max, depth and map
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  // count using fold
  def foldCount[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((a, b) => a + b + 1)

  // max using fold
  def foldMax(tree: Tree[Int]): Int = fold(tree)(a => a)((a, b) => a.max(b))

  // max using fold
  def foldDepth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((a, b) => 1 + a.max(b))

  // map using fold
  def foldMap[A, B](tree: Tree[A])(f : A => B): Tree[B] = fold(tree)(a => Leaf(f(a)):Tree[B])(Branch(_,_))
}