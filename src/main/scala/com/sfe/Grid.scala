package com.sfe

import scala.util.Random

case class Grid(width: Int, height: Int) {

  val gridSize: Coordinate = Coordinate(width, height)
  val xList: List[Int] = List.range(0, width)
  val yList: List[Int] = List.range(0, height)

  def allCoords: List[Coordinate] = yList.flatMap(y => xList.map(x => Coordinate(x, y)))

  def randomCoord: Coordinate = Random.shuffle(allCoords).head

}

