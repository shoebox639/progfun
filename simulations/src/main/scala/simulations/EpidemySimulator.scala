package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  def getAllPeopleInRoom(r: Int, c: Int): List[Person] = persons.filter(p => p.row == r && p.col == c)

  def in(percent: Int) = randomBelow(100) < percent

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val incubationPeriod = 6
    val sickPeriod = 14
    val immunePeriod = 16
    val fullRecoveryPeriod = 18

    val prevalenceRate = 1
    val transmissionRate = 40
    val mortalityRate = 25
    val flyRate = 1
    // to complete: additional parameters of simulation

  }
  import SimConfig._

  def mod(x: Int, y: Int) = (x + y) % y

  def move(p: Person): Unit = {
    def moveAction() = {
      afterDelay(randomBelow(5) + 1) {
        def confineToGrid(x: (Int, Int)) = (mod(x._1, roomRows), mod(x._2, roomColumns))
        def noSickOrDeadPeople(x: (Int, Int)): Boolean = {
          val (r, c) = x
          !getAllPeopleInRoom(r, c).exists(p => p.sick || p.dead)
        }

        val possible = List(
          (p.row + 1, p.col),
          (p.row - 1, p.col),
          (p.row, p.col + 1),
          (p.row, p.col - 1)).map(confineToGrid).filter(noSickOrDeadPeople)

        if (!p.dead) {
          if (!possible.isEmpty) {
            val (r, c) = possible(randomBelow(possible.length))

            //println(s"Persion ${p.id} is moving to ($r, $c)")
            p.setPosition(r, c)

            expose(p)
            wait(p)
          } 
          else {
            //println(s"Person ${p.id} didn't move!")
          }
        }
      }
    }
    p addAction moveAction
  }

  def fly(p: Person): Unit = {
    p addAction { () => afterDelay(randomBelow(5) + 1) {

        p.row = randomBelow(roomRows)
        p.col = randomBelow(roomColumns)

        expose(p)
        wait(p)
      }
    }
  }

  def wait(p: Person): Unit = {
    def afterMoveAction() = {
      afterDelay(randomBelow(5) + 1) {
        if (!p.dead) {
          move(p)
        }
      }
    }
    p addAction afterMoveAction
  }

  def infect(p: Person): Unit = {
    p.infected = true
    sick(p)
    die(p)
    immune(p)
    healthy(p)
  }

  def sick(p: Person) = {
    p addAction { () =>
      afterDelay(incubationPeriod) { p.sick = true; }
    }
  }

  def die(p: Person) = {
    p addAction { () =>
      afterDelay(sickPeriod) {
        if (in(mortalityRate)) p.dead = true;
      }
    }
  }

  def immune(p: Person) = {
    p addAction { () =>
      afterDelay(immunePeriod) {
        if (!p.dead) {
          p.immune = true;
          p.sick = false;
        }
      }
    }
  }

  def healthy(p: Person) = {
    p addAction { () =>
      afterDelay(fullRecoveryPeriod) {
        if (!p.dead) {
          p.infected = false;
          p.immune = false;
        }
      }
    }
  }

  def expose(p: Person) {
    for (inf <- p.roommates.filter(p => p.infected)) {
      if (!(p.infected || p.immune) && in(transmissionRate)) infect(p)
    }
  }

  val persons: List[Person] = (for (i <- 1 to population) yield new Person(i)).toList

  for (p <- persons) {
    move(p)
    if (p.id % 100 == prevalenceRate) infect(p)
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var actions: List[Simulator#Action] = List()

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def addAction(a: Simulator#Action) {
      actions = a :: actions
      a()
    }

    def setPosition(r: Int, c: Int) = {
      row = r;
      col = c;
    }

    def roommates = getAllPeopleInRoom(row, col)

    //
    // to complete with simulation logic
    //
  }
}
