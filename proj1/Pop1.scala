package com.sutol.scalgen.proj1

import com.sutol.scalgen.generics.Population

// Created by sutol on 14/04/2016. Part of scalgen.

class Pop1(newPopSize: Int = 10, newGeneCount: Int = 10, newEliteCount: Int = 2, newRanSeed: Int = 524)
  extends Link1 with Population {
    val populationSize: Int = newPopSize
    val geneCount: Int = newGeneCount
    val elitismCount: Int = newEliteCount
    val randomSeed: Int = newRanSeed

    def haltCond: Boolean = {
        currentGen >= 30
    }

}
