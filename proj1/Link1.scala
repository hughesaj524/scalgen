package com.sutol.scalgen.proj1

import com.sutol.scalgen.generics.GeneLink

// Created by sutol on 14/04/2016. Part of Omen'Ya.

trait Link1 extends GeneLink {
    type C = Chrom1
    type P = Pop1

    def newC(parent: Pop1, genes: Int = -1): Chrom1 = new Chrom1(parent, genes)
}
