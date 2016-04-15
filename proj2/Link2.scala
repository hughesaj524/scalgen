package com.sutol.scalgen.proj2

import com.sutol.scalgen.generics.GeneLink

// Created by sutol on 14/04/2016. Part of scalgen.

trait Link2 extends GeneLink {
    type C = Chrom2
    type P = Pop2

    def newC(parent: Pop2, genes: Int = -1): Chrom2 = new Chrom2(parent, genes)
}
