package com.sutol.scalgen.generics

// Created by sutol on 12/04/2016. Part of scalgen.

trait GeneLink {
    type C <: Chromosome with Ordered[C]
    type P <: Population

    def newC(parent: P, genes: Int = -1): C = new C()
}
