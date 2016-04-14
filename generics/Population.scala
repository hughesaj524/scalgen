package com.sutol.scalgen.generics

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks._

// Created by sutol on 30/03/2016. Part of scalgen.

/** A generic controller class for genetic algorithms */
trait Population extends GeneLink {
    /** The target population member. */
    val target: C
    /** The seed used to create a random generator. Ensures simulations are identical for easier testing; consistent
      * randomness lets you see effects of changes more clearly. Alternately, use a randomly generated value
      * (or something random-ish) for random simulations. */
    var randomSeed: Int
    /** The number of genes contained in one chromosome. */
    var geneCount: Int
    /** The number of members in the population. MUST BE EVEN because I am bad at scala. */
    //TODO: fix this ↑ population count only being even due to newPop iterator
    var populationSize: Int
    /** The chance out of 1 that each gene will mutate. Around 0.0015 is good. */
    var mutateChance: Double
    /** The number of "best" solutions copied from one generation to the next. 0 is fine. */
    var elitismCount: Int

    /** The population of chromosomes. It should be an array, but generic types are a nightmare. */
    //TODO: fix this ↑ population being a list
    var population = List[C]()
    var bestSolution: C
    protected var currentGen = 0
    /** The random number generator. See randomSeed for more info. */
    val seededRandom = new Random(randomSeed)

    /** Checks if the current generation is the final one.
      *
      * @return True if the current generation is final (or ideally, but not necessarily, past that)
      */
    def haltCond: Boolean

    /** Advances the population by one generation.
      *
      * @return False if finished, otherwise true
      */
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

    /** Produces a new generation of chromosomes from the current one. Uses UX and roulette selection by default. */
    def newPop(): Unit = {
        val nextPop = new ListBuffer[C]

        //Copied elitism chromosomes
        for (i <- 0 until elitismCount) {
            nextPop(i) = population(i)
        }

        val weightedPop = weightPop

        //TODO: Split this off into a roulette selection function.
        var parent1 = newC(this.asInstanceOf[P])
        var parent2 = newC(this.asInstanceOf[P])
        var roulette: Double = 0d
        var children: (C, C) = null

        //choose two parents, crossover producing two children, which join new population.
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

            children = (parent1 UX parent2).asInstanceOf[Tuple2[C, C]]
            nextPop(index * 2) = children._1
            nextPop(index * 2 - 1) = children._2
        }
        population = nextPop.toList.sorted
    }

    /** Create a new map with weighted values for roulette wheel selection.
      *
      * @return A map of weighted floats paired with chromosomes.
      */
    def weightPop: Map[Double, C] = {
        var index = 0
        var weightedPop = Map[Double, C]()
        for (i <- population.indices) {
            index += i
            weightedPop += (((1 / i).asInstanceOf[Double], population(i)))
        }
        weightedPop
    }

    /** Syntactical sugar; finds the best solution in an array as an array of boolean values for pretty abstraction
      *
      * @return The best solution's alleles as an array of boolean values.
      */
    def getBest: Array[Boolean] = population.head.genesToArray
}
