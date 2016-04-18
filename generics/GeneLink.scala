package com.sutol.scalgen.generics

import scala.collection.mutable

// Created by sutol on 12/04/2016. Part of scalgen.

/** A trait allowing for easier generic types in the generic classes.
  * Your Chromosome and Population classes should extend it.
  */
trait GeneLink {
    /** The chromosome type. */
    type C <: Chromosome with Ordered[C]
    /** The population type. */
    type P <: Population

    /** The constructor for the chromosome class.
      *
      * NOTE: It's not enforced, but it's recommended to define a newC for arrays as well.
      *
      * @param parent The parent of the chromosome.
      * @param genes  The genes of the chromosome. Can be omitted.
      * @return A new chromosome with the genes and parent specified.
      */
    def newC(parent: P, genes: mutable.BitSet = mutable.BitSet.empty): C
}
