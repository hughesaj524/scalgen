package scalgen.generics

// Created by sutol on 28/03/2016. Part of scalagen.


abstract class Chromosome(parent: Population, geneSet: Int = -1) extends Ordered[Chromosome] {
    val geneCount = parent.geneCount
    var genes: Int = 0

    if (geneSet == -1) {
        genes = parent.seededRandom.nextInt((1 << geneCount) - 1)
    } else {
        genes = geneSet
    }

    //Gets the allele of a gene at a specific index.
    def geneAt(index: Int): Boolean = {
        val bit = this.genes & (1 << index) >> index
        bit.asInstanceOf[Boolean]
    }

    //Produces a child from the genes of this chromosome and another.
    def UX(that: this.type): (this.type, this.type) = {
        val parent1 = this.genesToArray
        val parent2 = that.genesToArray
        val newGenes1 = new Array[Boolean](geneCount)
        val newGenes2 = new Array[Boolean](geneCount)

        for (i <- 0 until geneCount) {
            if (parent.seededRandom.nextBoolean) {
                newGenes1(i) = parent1(i)
                newGenes2(i) = parent2(i)
            } else {
                newGenes2(i) = parent1(i)
                newGenes1(i) = parent2(i)
            }
        }

        val child1 = new parent.T(parent, geneCount)
        val child2 = new this.type(parent, geneCount)
        child1.arrayToGenes(newGenes1)
        child2.arrayToGenes(newGenes2)
        child1.mutate()
        child2.mutate()
        (child1, child2)
    }

    //Mutates the chromosome according to the population's mutation chance.
    def mutate(): Unit = {
        val newGenes: Array[Boolean] = genesToArray
        for (i <- 0 until geneCount)
            if (parent.seededRandom.nextDouble() > parent.mutateChance) {
                newGenes(i) = !genesToArray(i)
            }
        arrayToGenes(newGenes)
    }

    //Returns the current chromosome's genes as an array of boolean values representing the current alleles.
    def genesToArray: Array[Boolean] = {
        val bitArray = new Array[Boolean](geneCount)
        for (i <- 0 until geneCount) {
            if (((1 << i) & genes) == (1 << i)) {
                bitArray(i) = true
            } else {
                bitArray(i) = false
            }
        }
        bitArray
    }

    //Sets an array of boolean values to the current chromosome's genes.
    def arrayToGenes(arrayOfBits: Array[Boolean]): Int = {
        var convertedGenes: Int = 0
        for (i <- arrayOfBits.indices) {
            if (arrayOfBits(i)) {
                convertedGenes = convertedGenes | (1 << i)
            }
        }
        convertedGenes
    }

    def compare(that: this.type) = this.getFitness - that.getFitness

    //Calculates the fitness value of the chromosome.
    def getFitness: Int = {
        var fitness: Int = 0
        for (i <- 0 until geneCount) {
            if (((1 << i) & genes) == ((1 << i) & parent.target.genes)) {
                fitness += 1
            }
        }
        fitness
    }
}
