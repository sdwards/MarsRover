package com.sfe

object Main extends App {
  val grid = Grid(10, 10)
  val roverPosition = Coordinate(1, 2)
  val destination = Coordinate(8, 5)
  val mountainRange = List(Coordinate(3, 4), Coordinate(3, 5), Coordinate(3, 6), Coordinate(3, 7))
  val rover1 = PathFinder(grid, roverPosition, destination, mountainRange)

  Print(rover1)


}