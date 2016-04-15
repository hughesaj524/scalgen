package com.sutol.scalgen.proj2

import com.sutol.scalgen.generics.Population

// Created by sutol on 14/04/2016. Part of scalgen.

class Pop2 extends Link2 with Population {
    val populationSize: Int = 30
    // 4 bits for pop size, 4 for elitism count
    val geneCount: Int = 8
    val elitismCount: Int = 0
    val randomSeed: Int = System.currentTimeMillis().toInt
    mutateChance *= 1000

    def haltCond: Boolean = {
        this.currentGen >= 30
    }
}
