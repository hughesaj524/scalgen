package com.sutol.scalgen.proj2

// Created by sutol on 14/04/2016. Part of scalgen.

object proj2 {
    def main(args: Array[String]) {
        val pop = new Pop2()
        var num1: Int = 0
        var num2: Int = 0
        while (pop.step()) {
            num1 = pop.getBest.genes & 1111
            num2 = pop.getBest.genes >> 4
            println(num1.toString + " " + num2.toString + " : " + pop.getBest.getFitness.toString)
        }
    }
}
