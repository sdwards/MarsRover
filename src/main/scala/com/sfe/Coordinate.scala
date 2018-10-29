package com.sfe

case class Coordinate(x: Int, y: Int) {

  def +(other: Coordinate): Coordinate = Coordinate.addition(this, other)

  def -(other: Coordinate): Coordinate = Coordinate.subtraction(this, other)

  def %(other: Coordinate): Coordinate = Coordinate.modulus(this, other)

  def noOfMoves: Int = Coordinate.noOfMoves(this)

  def contains(n: Int): Boolean = Coordinate.contains(this, n)

}

object Coordinate {

  def addition(a: Coordinate, b: Coordinate): Coordinate = Coordinate(a.x + b.x, a.y + b.y)

  def subtraction(a: Coordinate, b: Coordinate): Coordinate = Coordinate(a.x - b.x, a.y - b.y)

  def modulus(a: Coordinate, b: Coordinate): Coordinate = Coordinate(((a.x % b.x) + b.x) % b.x, ((a.y % b.y) + b.y) % b.y)

  def noOfMoves(a: Coordinate): Int = Math.abs(a.x) + Math.abs(a.y)

  def contains(a: Coordinate, n: Int): Boolean = a.x == n || a.y == n

  val identity: Coordinate = Coordinate(0, 0)
  val Up: Coordinate = Coordinate(0, 1)
  val Down: Coordinate = Coordinate(0, -1)
  val Left: Coordinate = Coordinate(-1, 0)
  val Right: Coordinate = Coordinate(1, 0)


}

case class WeightedCoord(coord: Coordinate, weight: Int)