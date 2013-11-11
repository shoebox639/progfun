package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
    val sb = startBlock
  }
  
  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("isStanding both on same tile -> true") {
    new Level1 {
      assert(Block(new Pos(0, 0), new Pos(0, 0)).isStanding)
    }
  }

  test("isStanding on diff tiles -> false") {
    new Level1 {
      assert(!Block(new Pos(0, 0), new Pos(0, 1)).isStanding)
    }
  }

  test("isLegal standing and completely on -> true") {
    new Level1 {
      assert(Block(new Pos(0, 0), new Pos(0, 0)).isLegal)
    }
  }

  test("isLegal lying and completely on -> true") {
    new Level1 {
      assert(Block(new Pos(0, 0), new Pos(0, 1)).isLegal)
    }
  }

  test("isLegal standing and completely off -> false") {
    new Level1 {
      assert(!Block(new Pos(1, 6), new Pos(1, 6)).isLegal)
    }
  }

  test("isLegal lying and completely off -> false") {
    new Level1 {
      assert(!Block(new Pos(1, 6), new Pos(1, 7)).isLegal)
    }
  }

  test("isLegal lying and completely off board -> false") {
    new Level1 {
      assert(!Block(new Pos(0, 20), new Pos(0, 21)).isLegal)
    }
  }

  test("isLegal standing and completely off board -> false") {
    new Level1 {
      assert(!Block(new Pos(0, 20), new Pos(0, 20)).isLegal)
    }
  }

  test("isLegal lying and partially off -> false") {
    new Level1 {
      assert(!Block(new Pos(1, 5), new Pos(1, 6)).isLegal)
    }
  }

  test("startBlock") {
    new Level1 {
      assert(sb.b1 == startPos)
      assert(sb.b2 == startPos)
    }
  }

  /**
   *  ooo-------
   * oSoooo----
   * ooooooooo-
   * -ooooooooo
   * -----ooToo
   * ------ooo-
   */
  test("neighbors at start") {
    new Level1 {
      val neighbors = sb.neighbors.distinct
      val expected = List(
        (new Block(new Pos(1, -1), new Pos(1, 0)), Left),
        (new Block(new Pos(1, 2), new Pos(1, 3)), Right),
        (new Block(new Pos(-1, 1), new Pos(0, 1)), Up),
        (new Block(new Pos(2, 1), new Pos(3, 1)), Down)).distinct

      assert(expected == neighbors, neighbors)
    }
  }

  test("legal neighbors at start") {
    new Level1 {
      val neighbors = sb.legalNeighbors.distinct
      val expected = List(
        (new Block(new Pos(1, 2), new Pos(1, 3)), Right),
        (new Block(new Pos(2, 1), new Pos(3, 1)), Down)).distinct

      assert(expected == neighbors, neighbors)
    }
  }

  test("done: at start -> false") {
    new Level1 {
      assert(!done(sb))
    }
  }

  test("done: at goal -> true") {
    new Level1 {
      val atEnd = new Block(new Pos(4, 7), new Pos(4, 7))
      assert(done(atEnd))
    }
  }

  test("neighborsWithHistory: example test") {
    new Level1 {
      val actual = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val expected = Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream

      assert(actual == expected, ("expected" -> expected, "actual" -> actual))
    }
  }

  test("newNeighborsOnly: example test") {
    new Level1 {
      val actual = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))

      val expected = Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream

      assert(actual == expected, ("expected" -> expected, "actual" -> actual))
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
