package scalgen.generics

import scala.util.control.Breaks._
import scala.util.{Random, Sorting}

// Created by sutol on 30/03/2016. Part of scalgen.

/** A generic controller class for genetic algorithms
  *
  */
trait Population extends GeneLink {
    /** The random number generator. Ensures simulations are identical for easier testing; consistent randomness lets
      * you see effects of changes more clearly. Alternately, use a randomly generated value (or something random-ish)
      * for random simulations.
      */
    val seededRandom = new Random(randomSeed)
    /** The target population member. */
    val target: C
    /** The seed used to create a random generator. See seededRandom for more info. */
    var randomSeed: Int
    /** The number of genes contained in one chromosome. */
    var geneCount: Int
    /** The number of members in the population. MUST BE EVEN because I am bad at scala. */
    //TODO: fix this ↑
    var populationSize: Int
    /** The chance out of 1 that each gene will mutate. Around 0.0015 is good. */
    var mutateChance: Double
    /** The number of "best" solutions copied from one generation to the next. 0 is fine. */
    var elitismCount: Int
    var population = new Array[C](populationSize)
    var bestSolution = new C(this)
    protected var currentGen = 0

    /** Checks if the current generation is the final one.
      *
      * @return True if the current generation is final (or ideally, but not necessarily, past that)
      */
    def haltCond: Boolean

    def step(): Boolean = {
        if (haltCond) {
            false
        } else {
            if (population.length == 0) {
                for (i <- 0 until populationSize) {
                    population(i) = new C(this)
                }
            } else {
                newPop()
            }

            population.length == populationSize
        }
    }

    def newPop(): Unit = {
        val nextPop = new Array[C](populationSize)
        Sorting.quickSort[C](population)

        //Copied elitism chromosomes
        for (i <- 0 until elitismCount) {
            nextPop(i) = population(0)
        }

        val weightedPop = weightPop

        var parent1 = new C(this)
        var parent2 = new C(this)
        var roulette: Double = 0d
        var children: (C, C) = null
        for (index <- 0 until (populationSize / 2)) {
            roulette = seededRandom.nextDouble

            breakable {
                for (i <- weightedPop.keys) {
                    if (roulette >= i) {
                        parent1 = weightedPop(i)
                        break
                    }
                }
            }

            breakable {
                for (i <- weightedPop.keys) {
                    if (roulette >= i) {
                        parent2 = weightedPop(i)
                        break
                    }
                }
            }

            children = parent1 UX parent2
            nextPop(index * 2) = children._1
            nextPop(index * 2 - 1) = children._2
        }
    }

    //Created a new map with weighted values. See roulette wheel selection.
    def weightPop: Map[Double, C] = {
        var index = 0
        var weightedPop = Map[Double, C]()
        for (i <- population.indices) {
            index += i
            weightedPop += (((1 / i).asInstanceOf[Double], population(i)))
        }
        weightedPop
    }

    def getBest: Array[Boolean] = {
        Sorting.quickSort[C](population)
        population(0).genesToArray
    }

    def wrapArray(array: Array[Boolean], length: Int): Array[Array[Array[Boolean]]] = {
        val newArray = Array.ofDim[Boolean](length, length, length)
        var counter = 0
        for (ix <- 0 until length) {
            for (iy <- 0 until length) {
                for (iz <- 0 until length) {
                    newArray(ix)(iy)(iz) = array(counter)
                    counter += 1
                }
            }
        }
        newArray
    }
}