package com.sfe

case class Print(pathFinder: PathFinder) {

  val grid: Unit = {
    val allCoords = pathFinder.grid.allCoords.reverse

    def getRow(all: List[Coordinate]): Unit = {
      if (all == Nil) println()
      else {
        val row = all.take(pathFinder.grid.width).reverse
        println(printRow(row))
        getRow(all diff row)
      }
    }

    def printRow(row: List[Coordinate]): String =
      row match {
        case Nil => ""
        case list => {
          if (list.head == pathFinder.start) "S  " + printRow(row.tail)
          else if (list.head == pathFinder.end) "E  " + printRow(row.tail)
          else if (pathFinder.obstacles.contains(list.head)) "X  " + printRow(row.tail)
          else "*  " + printRow(row.tail)
        }
      }

    getRow(allCoords)
  }

  val directions: Unit = pathFinder.directions.zip(pathFinder.path.tail).foreach(a => println(s"${a._1} -  ${a._2}"))

}
