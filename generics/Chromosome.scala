package com.sutol.scalgen.generics

// Created by sutol on 28/03/2016. Part of scalgen.

/** A generic chromosome class for genetic algorithms.
  *
  * The population is made up of these. These, in turn, are made up of genes, which can be in a series of different
  * states, or alleles. The genes are actually stored as a singe integer for efficiency reasons, but there are plenty
  * of functions to abstract that away if you don't want to deal with it.
  */
trait Chromosome extends Ordered[Chromosome] with GeneLink {
    var genes: Int
    var parent: P

    /** Initializes the chromosome. You should ABSOLUTELY call this in your class constructor, or unexpected things
      * might happen!
      *
      * @param newParent The population to be assigned as the parent of the chromosome.
      * @param geneSet   The set of genes for the chromosome. Can be omitted.
      */
    def init(newParent: P, geneSet: Int = -1): Unit = {
        this.parent = newParent
        this.setGenes(geneSet)
    }

    /** Sets the genes of the chromosome.
      *
      * @param geneSet An integer representing the genes of the chromosome.
      */
    def setGenes(geneSet: Int): Unit = {
        if (geneSet == -1) {
            this.genes = parent.seededRandom.nextInt((1 << parent.geneCount) - 1)
        } else {
            this.genes = geneSet
        }
    }

    /** Sets the genes of the chromosome.
      *
      * @param geneSet An array of booleans representing the alleles of the genes of the chromosome.
      */
    def setGenes(geneSet: Array[Boolean]): Unit = {
        this.genes = arrayToGenes(geneSet)
    }

    /** Sets an array of boolean values to the current chromosome's genes.
      *
      * @param arrayOfBits An array of booleans
      * @return The array of booleans as represented by an integer.
      */
    def arrayToGenes(arrayOfBits: Array[Boolean]): Int = {
        var convertedGenes: Int = 0
        for (i <- arrayOfBits.indices) {
            if (arrayOfBits(i)) {
                convertedGenes = convertedGenes | (1 << i)
            }
        }
        convertedGenes
    }

    /** Returns the allele of a gene at a specific index.
      *
      * @param index The index of the gene to be returned.
      * @return The allele of the gene at the specified index.
      */
    def geneAt(index: Int): Boolean = {
        val bit = this.genes & (1 << index) >> index
        bit.asInstanceOf[Boolean]
    }

    /** Produces a child from the genes of this chromosome and another through uniform crossover.
      *
      * @param that The other chromosome for crossover.
      * @return A tuple of two chromosomes produced by the crossover
      * @tparam T The type of the parameter / result. Ideally this wouldn't be necessary...
      */
    //TODO: Make this not require a tparam
    def UX[T <: Chromosome](that: T): (C, C) = {
        val parent1 = this.genesToArray
        val parent2 = that.genesToArray
        val newGenes1 = new Array[Boolean](parent.geneCount)
        val newGenes2 = new Array[Boolean](parent.geneCount)

        for (i <- 0 until parent.geneCount) {
            if (parent.seededRandom.nextBoolean) {
                newGenes1(i) = parent1(i)
                newGenes2(i) = parent2(i)
            } else {
                newGenes2(i) = parent1(i)
                newGenes1(i) = parent2(i)
            }
        }

        val child1 = newC(parent)
        val child2 = newC(parent)
        child1.arrayToGenes(newGenes1)
        child2.arrayToGenes(newGenes2)
        child1.mutate()
        child2.mutate()
        (child1, child2)
    }

    /** Mutates the chromosome according to the population's mutation chance. */
    def mutate(): Unit = {
        val newGenes: Array[Boolean] = genesToArray
        for (i <- 0 until parent.geneCount)
            if (parent.seededRandom.nextDouble() > parent.mutateChance) {
                newGenes(i) = !genesToArray(i)
            }
        arrayToGenes(newGenes)
    }

    /** Returns the current chromosome's genes as an array of boolean values representing the current alleles.
      *
      * @return An array of booleans which represents the genes of the chromosome.
      */
    def genesToArray: Array[Boolean] = {
        val bitArray = new Array[Boolean](parent.geneCount)
        for (i <- 0 until parent.geneCount) {
            if (((1 << i) & genes) == (1 << i)) {
                bitArray(i) = true
            } else {
                bitArray(i) = false
            }
        }
        bitArray
    }

    /** Returns the difference between this and another chromosome's fitness.
      *
      * Inherited from Ordered, so primarily for sorting, but it's probably fine to use it for anything else! Just
      * keep in mind it negates the result to ensure the fittest chromosome is first.
      */
    def compare(that: C) = -(this.getFitness - that.getFitness)

    /** Returns the fitness value of the chromosome.
      *
      * @return The fitness of the chromosome as an integer.
      */
    def getFitness: Int = {
        var fitness: Int = 0
        for (i <- 0 until parent.geneCount) {
            if (((1 << i) & genes) == ((1 << i) & parent.target.genes)) {
                fitness += 1
            }
        }
        fitness
    }
}
