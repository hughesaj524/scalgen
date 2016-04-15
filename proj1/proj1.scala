package com.sutol.scalgen.proj1

// Created by sutol on 14/04/2016. Part of scalgen.

object proj1 {
    def main(args: Array[String]) {
        val pop = new Pop1
        while (pop.step()) {
            println(pop.getBest.genes)
        }
    }
}
