package com.sutol.scalgen.generics

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks._

// Created by sutol on 30/03/2016. Part of scalgen.

/** A generic controller class for genetic algorithms
  *
  */
trait Population extends GeneLink {
    /** The random number generator. See randomSeed for more info. */
    val seededRandom = new Random(randomSeed)
    /** The target population member. */
    val target: C
    /** The seed used to create a random generator. Ensures simulations are identical for easier testing; consistent
      * randomness lets you see effects of changes more clearly. Alternately, use a randomly generated value
      * (or something random-ish) for random simulations. */
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
    var population = List[C]()
    var bestSolution: C
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
            var popBuffer = new ListBuffer[C]
            if (population.isEmpty) {
                for (i <- 0 until populationSize) {
                    popBuffer += newC(this.asInstanceOf[P])
                }
                population = popBuffer.toList.sorted
            } else {
                newPop()
            }

            population.length == populationSize
        }
    }

    def newPop(): Unit = {
        val nextPop = new ListBuffer[C]

        //Copied elitism chromosomes
        for (i <- 0 until elitismCount) {
            nextPop(i) = population(i)
        }

        val weightedPop = weightPop

        var parent1 = newC(this.asInstanceOf[P])
        var parent2 = newC(this.asInstanceOf[P])
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
        population.head.genesToArray
    }
}
