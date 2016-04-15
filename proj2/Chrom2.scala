package com.sutol.scalgen.proj2


import com.sutol.scalgen.generics.Chromosome
import com.sutol.scalgen.proj1.Pop1

// Created by sutol on 14/04/2016. Part of scalgen.

class Chrom2(newParent: Pop2, newGenes: Int = -1) extends Link2 with Chromosome with Ordered[Chrom2] {
    var parent: P = _
    init(newParent, newGenes)

    private var fitness: Int = -30

    //I spent too long naming this. No regrets.
    var spawn: Pop1 = _
    def compare(that: Chrom2): Int = { -(this.getFitness - that.getFitness) }

    override def getFitness: Int = {
        if (fitness == -30) {
            val popSize: Int = genes >> 4 //the last 4 bits of the number.
            val eliteCount: Int = genes & 15 //1111; the first 4 bits of the number.

            if (eliteCount > popSize) {
                return popSize - eliteCount
            }

            spawn = new Pop1(newPopSize = popSize, newEliteCount = eliteCount)

            while (spawn.step()) {}

            var total: Int = 0
            for (i <- spawn.population) {
                total += i.genes
            }
            try {
                fitness = total / spawn.populationSize
            } catch {
                case ae: ArithmeticException => fitness = 0
            }
        }
        fitness
    }
}
