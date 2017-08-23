package com.deameamo.rst

import com.deameamo.util.ArrayList

import scala.collection.mutable

object Test {
  
  def main(args: Array[String]) {
    val ps = new ArrayList[Int]
    ps += 0
    ps += 3
    ps += 1
    ps += 2
    val sorted = ps.sortWith((a, b) => {
      if(a < b) true else false
    })
    println(sorted)
  }
  
  def anchor(num: Int, safe: (Int, Int) => Boolean) {
    if(safe(num, 10))
      println(num)
    else
      println(safe(num, 10))
  }
  
  def limit(x: Int): Boolean = largerThan(x)(0)
  
  def largerThan(a: Int)(b: Int): Boolean = a > b
}

class Test {

  def choose(ps: mutable.MutableList[Double]): Int = {
    val ranges = new mutable.MutableList[Range]
    var total = 0.0
    ps.foreach { p => {
      ranges += Range(total, total + p)
      total += p
    }}
    val seed = Math.random * total
    var i = 0
    var choice = -1
    while(i < ranges.size && choice == -1) {
      val range = ranges.get(i).get
      if(range.cover(seed)) {
        choice = i
      }
      i += 1
    }
    choice
  }
}

case class Range(from: Double, to: Double) {
  def cover(d: Double): Boolean = from <= d && d < to
}