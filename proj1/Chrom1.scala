package com.sutol.scalgen.proj1

import com.sutol.scalgen.generics.Chromosome

// Created by sutol on 14/04/2016. Part of Omen'Ya.

class Chrom1(newParent: Pop1, newGenes: Int = -1) extends Link1 with Chromosome with Ordered[Chrom1] {
    var parent: P = newParent
    genes = newGenes

    def compare(that: Chrom1): Int = {
        -(this.getFitness - that.getFitness)
    }

    override def getFitness: Int = genes
}
