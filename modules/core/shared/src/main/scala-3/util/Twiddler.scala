// Copyright (c) 2018-2021 by Rob Norris
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package skunk
package util

import scala.annotation.implicitNotFound
import scala.quoted._
import scala.deriving.Mirror

import skunk.implicits._

/** Witness that type `A` is isomorphic to a twiddle list. */
@implicitNotFound("Cannot construct a mapping between the source (which must be a twiddle-list type) and the specified target type ${A} (which must be a case class of the same structure).")
trait Twiddler[A] {
  type Out
  def to(h: A): Out
  def from(o: Out): A
}

object Twiddler {

  def apply[H](implicit ev: Twiddler[H]): ev.type = ev

  type Aux[A, O] = Twiddler[A] { type Out = O }

  implicit def product1[P <: Product, A](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= A *: EmptyTuple
    ): Twiddler[P] { type Out = A  } =
      new Twiddler[P] {
        type Out = A
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match { case a *: EmptyTuple => a }
        def from(o: Out): P = o match { case a => m.fromProduct(a *: EmptyTuple) }
      }

  implicit def product2[P <: Product, A, B](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B)
    ): Twiddler[P] { type Out = A ~ B  } =
      new Twiddler[P] {
        type Out = A ~ B
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match { case (a, b) => a ~ b }
        def from(o: Out): P = o match { case a ~ b => m.fromProduct((a, b)) }
      }

  implicit def product3[P <: Product, A, B, C](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C)
    ): Twiddler[P] { type Out = A ~ B ~ C } =
      new Twiddler[P] {
        type Out = A ~ B ~ C
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match { case (a, b, c) => a ~ b ~ c }
        def from(o: Out): P = o match { case a ~ b ~ c => m.fromProduct((a, b, c)) }
      }

  implicit def product4[P <: Product, A, B, C, D](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match { case (a, b, c, d) => a ~ b ~ c ~ d }
        def from(o: Out): P = o match { case a ~ b ~ c ~ d => m.fromProduct((a, b, c, d)) }
      }

  implicit def product5[P <: Product, A, B, C, D, E](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match { case (a, b, c, d, e) => a ~ b ~ c ~ d ~ e }
        def from(o: Out): P = o match { case a ~ b ~ c ~ d ~ e => m.fromProduct((a, b, c, d, e)) }
      }

  implicit def product6[P <: Product, A, B, C, D, E, F](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match { case (a, b, c, d, e, f) => a ~ b ~ c ~ d ~ e ~ f }
        def from(o: Out): P = o match { case a ~ b ~ c ~ d ~ e ~ f => m.fromProduct((a, b, c, d, e, f)) }
      }
  
  implicit def product7[P <: Product, A, B, C, D, E, F, U](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F, U)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F ~ U } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F ~ U
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match {
          case (a, b, c, d, e, f, u) => a ~ b ~ c ~ d ~ e ~ f ~ u }
        def from(o: Out): P = o match {
          case a ~ b ~ c ~ d ~ e ~ f ~ u => m.fromProduct((a, b, c, d, e, f, u)) }
      }

  implicit def product8[P <: Product, A, B, C, D, E, F, U, V](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F, U, V)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match {
          case (a, b, c, d, e, f, u, v) => a ~ b ~ c ~ d ~ e ~ f ~ u ~ v }
        def from(o: Out): P = o match {
          case a ~ b ~ c ~ d ~ e ~ f ~ u ~ v => m.fromProduct((a, b, c, d, e, f, u, v)) }
      }

  implicit def product9[P <: Product, A, B, C, D, E, F, U, V, W](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F, U, V, W)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match {
          case (a, b, c, d, e, f, u, v, w) => a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w }
        def from(o: Out): P = o match {
          case a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w => m.fromProduct((a, b, c, d, e, f, u, v, w)) }
      }

  implicit def product10[P <: Product, A, B, C, D, E, F, U, V, W, X](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F, U, V, W, X)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W ~ X} =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W ~ X
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match {
          case (a, b, c, d, e, f, u, v, w, x) => a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w ~ x }
        def from(o: Out): P = o match {
          case a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w ~ x => m.fromProduct((a, b, c, d, e, f, u, v, w, x)) }
      }

  implicit def product11[P <: Product, A, B, C, D, E, F, U, V, W, X, Y](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F, U, V, W, X, Y)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W ~ X ~ Y } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W ~ X ~ Y
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match {
          case (a, b, c, d, e, f, u, v, w, x, y) => a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w ~ x ~ y }
        def from(o: Out): P = o match {
          case a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w ~ x ~ y => m.fromProduct((a, b, c, d, e, f, u, v, w, x, y)) }
      }

  implicit def product12[P <: Product, A, B, C, D, E, F, U, V, W, X, Y, Z](
    implicit m: Mirror.ProductOf[P],
             i: m.MirroredElemTypes =:= (A, B, C, D, E, F, U, V, W, X, Y, Z)
    ): Twiddler[P] { type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W ~ X ~ Y ~ Z } =
      new Twiddler[P] {
        type Out = A ~ B ~ C ~ D ~ E ~ F ~ U ~ V ~ W ~ X ~ Y ~ Z
        def to(p: P): Out = i(Tuple.fromProductTyped(p)) match {
          case (a, b, c, d, e, f, u, v, w, x, y, z) => a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w ~ x ~ y ~ z }
        def from(o: Out): P = o match {
          case a ~ b ~ c ~ d ~ e ~ f ~ u ~ v ~ w ~ x ~ y ~ z => m.fromProduct((a, b, c, d, e, f, u, v, w, x, y, z)) }
      }

}

