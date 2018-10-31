package com.sfe

case class PathFinder(grid: Grid, start: Coordinate, end: Coordinate, obstacles: List[Coordinate]) {

  def listOfEndCoords: List[Coordinate] = List(
    end,
    end + Coordinate(grid.width, 0),
    end - Coordinate(grid.width, 0),
    end + Coordinate(0, grid.height),
    end - Coordinate(0, grid.height))

  def findPath: List[Coordinate] = {

    import PathFinder._

    def loop(currentPosition: WeightedCoord, closed: List[WeightedCoord], open: List[WeightedCoord]): List[Coordinate] = {
      if (listOfEndCoords.contains(currentPosition.coord)) extractPathFromClosed(closed :+ currentPosition)
      else {
        val adjacentCoords: List[Coordinate] = filterEligibleForOpen(findAdjacent(currentPosition.coord), closed.map(wc => wc.coord) ++ obstacles)
        val newOpen: List[WeightedCoord] = open ++ calculateWeightedCoords(adjacentCoords, currentPosition.coord, start, listOfEndCoords)
        val newPosition: WeightedCoord = selectNextPosition(newOpen)
        val newOpenMinusNewPosition: List[WeightedCoord] = newOpen diff List(newPosition)
        loop(newPosition, closed :+ newPosition, newOpenMinusNewPosition)
      }
    }

    val weightedStart: WeightedCoord = WeightedCoord(start, (end - start).noOfMoves, start)
    loop(weightedStart, List(weightedStart), Nil)

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
          case Coordinate(0, 0) => loop(path.tail, directions :+ "NONE: position is already destination", path.head)
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

  def calculateWeightedCoords(remaining: List[Coordinate], current: Coordinate, start: Coordinate, listOfEnd: List[Coordinate]): List[WeightedCoord] =
    remaining.map(a => WeightedCoord(a, calculateWeight(a, start, listOfEnd), current))

  def calculateWeight(coord: Coordinate, start: Coordinate, listOfEnd: List[Coordinate]): Int =
    (start - coord).noOfMoves + listOfEnd.map(end => (coord - end).noOfMoves).min

  def selectNextPosition(open: List[WeightedCoord]): WeightedCoord =
    open.minBy(_.weight)

  def extractPathFromClosed(closed: List[WeightedCoord]): List[Coordinate] = {
    val revClosed = closed.reverse

    def loop(closed: List[WeightedCoord], path: List[WeightedCoord], current: WeightedCoord): List[Coordinate] = {
      if (closed == Nil) path.map(wc => wc.coord)
      else
        closed.head match {
          case WeightedCoord(c, w, p) if c == current.parent => loop(closed.tail, List(WeightedCoord(c, w, p)) ++ path, WeightedCoord(c, w, p))
          case _ => loop(closed.tail, path, current)
        }
    }

    loop(revClosed, List(revClosed.head), revClosed.head)
  }

}



