package com.sutol.scalgen.generics

import scala.collection.mutable

// Created by sutol on 28/03/2016. Part of scalgen.

/** A generic chromosome class for genetic algorithms.
  *
  * The population is made up of these. These, in turn, are made up of genes, which can be in a series of different
  * states, or alleles. The genes are actually stored as a singe integer for efficiency reasons, but there are plenty
  * of functions to abstract that away if you don't want to deal with it.
  */
trait Chromosome extends GeneLink {
    protected var genes: mutable.BitSet = _
    var parent: P

    /** Initializes the chromosome. You should ABSOLUTELY call this in your class constructor, or unexpected things
      * might happen!
      *
      * @param newParent The population to be assigned as the parent of the chromosome.
      * @param geneSet   The set of genes for the chromosome. Can be omitted.
      */
    def init(newParent: P, geneSet: mutable.BitSet = mutable.BitSet.empty): Unit = {
        this.parent = newParent
        this.setGenes(geneSet)
    }

    /** Sets the genes of the chromosome.
      *
      * @param geneSet A mutable bitSet representing the genes of the chromosome.
      * @throws IllegalArgumentException if the new gene set is too high for the gene count dictated by the population.
      */
    def setGenes(geneSet: mutable.BitSet): Unit = {
        if (geneSet.size > parent.geneCount) {
            throw new IllegalArgumentException("Invalid gene set for chromosome!")
        }
        this.genes = geneSet
    }

    def getGenes: mutable.BitSet = { genes }

    /** Sets the genes of the chromosome.
      *
      * @param geneSet An array of booleans representing the alleles of the genes of the chromosome.
      */
    def setGenes(geneSet: Array[Boolean]): Unit = {
        val newGenes = mutable.BitSet.empty
        for (i <- 0 until parent.geneCount) {
            if (geneSet(i)) {
                newGenes.add(i)
            }
        }
        setGenes(newGenes)
    }

    /** Returns the allele of a gene at a specific index.
      *
      * @param index The index of the gene to be returned.
      * @return The allele of the gene at the specified index.
      */
    def geneAt(index: Int): Boolean = {
        genes(index)
    }

    /** Produces a child from the genes of this chromosome and another through uniform crossover.
      *
      * @param that The other chromosome for crossover.
      * @return A tuple of two chromosomes produced by the crossover
      * @tparam T The type of the parameter / result. Ideally this wouldn't be necessary...
      */
    //TODO: Make this not require a tparam
    //TODO: Optimise
    //TODO: Add more crossover functions for different types of crossover
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
        val newGenes: mutable.BitSet = genes
        for (i <- 0 until parent.geneCount)
            if (parent.seededRandom.nextDouble() > parent.mutateChance) {
                if (genes(i)) {
                    genes.remove(i)
                } else {
                    genes.add(i)
                }
            }
        setGenes(newGenes)
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
                convertedGenes |= (1 << i)
            }
        }
        convertedGenes
    }

    /** Returns the current chromosome's genes as an array of boolean values representing the current alleles.
      *
      * @return An array of booleans which represents the genes of the chromosome.
      */
    def genesToArray: Array[Boolean] = {
        val bitArray = new Array[Boolean](parent.geneCount)
        for (i <- 0 until parent.geneCount) {
            bitArray(i) = genes(i)
        }
        bitArray
    }

    /** Returns the fitness value of the chromosome.
      *
      * @return The fitness of the chromosome as an integer.
      */
    def getFitness: Int
}
