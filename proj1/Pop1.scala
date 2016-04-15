package com.sutol.scalgen.proj1

import com.sutol.scalgen.generics.Population

// Created by sutol on 14/04/2016. Part of scalgen.

class Pop1 extends Link1 with Population {
    val populationSize: Int = 10
    val geneCount: Int = 10
    val elitismCount: Int = 2
    val randomSeed: Int = 524

    def haltCond: Boolean = {
        currentGen >= 30
    }
}
