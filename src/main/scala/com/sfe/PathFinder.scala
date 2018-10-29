package com.sfe

case class PathFinder(grid: Grid, start: Coordinate, end: Coordinate, obstacles: List[Coordinate]) {

  def listOfEndCoords(end: Coordinate): List[Coordinate] = List(
    end,
    end + Coordinate(grid.width, 0),
    end - Coordinate(grid.width, 0),
    end + Coordinate(0, grid.height),
    end - Coordinate(0, grid.height))

  def findPath: List[Coordinate] = {
    def loop(currentPosition: Coordinate, closed: List[Coordinate], open: List[WeightedCoord]): List[Coordinate] = {
      if (listOfEndCoords(end).contains(currentPosition)) closed :+ currentPosition
      else {
        import PathFinder._

        val adjacentCoords: List[Coordinate] = filterEligibleForOpen(findAdjacent(currentPosition), closed ++ obstacles)
        val newOpen: List[WeightedCoord] = open ++ calculateWeightedCoords(adjacentCoords, currentPosition, listOfEndCoords(end))
        val newPosition = selectNextPosition(newOpen)

        loop(newPosition, closed :+ currentPosition, newOpen)

      }
    }

    loop(start, Nil, Nil)
  }

  def path: List[Coordinate] = findPath.map(a => a % Coordinate(grid.width, grid.width))


  def directions: List[String] = {
    val pathCoords = findPath
    val start = pathCoords.head

    def loop(path: List[Coordinate], directions: List[String], previousCoord: Coordinate): List[String] = {
      if (path == Nil) directions
      else {
        path.head - previousCoord match {
          case Coordinate(0, 1) => loop(path.tail, directions :+ "UP", path.head)
          case Coordinate(0, -1) => loop(path.tail, directions :+ "DOWN", path.head)
          case Coordinate(1, 0) => loop(path.tail, directions :+ "RIGHT", path.head)
          case Coordinate(-1, 0) => loop(path.tail, directions :+ "LEFT", path.head)
        }
      }
    }

    loop(pathCoords.tail, Nil, start)

  }

}

object PathFinder {

  def findAdjacent(a: Coordinate): List[Coordinate] =
    List(Coordinate.Up, Coordinate.Down, Coordinate.Left, Coordinate.Right).map(_ + a)

  def filterEligibleForOpen(possibles: List[Coordinate], invalids: List[Coordinate]): List[Coordinate] =
    possibles diff invalids

  def calculateWeightedCoords(remaining: List[Coordinate], start: Coordinate, listOfEnd: List[Coordinate]): List[WeightedCoord] =
    remaining.map(a => WeightedCoord(a, calculateWeight(a, start, listOfEnd)))

  def calculateWeight(coord: Coordinate, start: Coordinate, listOfEnd: List[Coordinate]): Int =
    (coord - start).noOfMoves + listOfEnd.map(end => (coord-end).noOfMoves).min

  def selectNextPosition(open: List[WeightedCoord]): Coordinate =
    open.minBy(_.weight).coord

}



