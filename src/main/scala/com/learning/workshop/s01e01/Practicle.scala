package com.learning.workshop.s01e01

import scala.annotation.tailrec
import scala.collection.mutable

object Practicle extends App {
  //println("Summ tradisional way: " + SummationTrad.summ(1, 1000000))
  println("Summ tradisional way: " + SummationTrad.summR(1, 10))
  println("Summ tradisional way: " + SummationTrad.summCubes(1, 5))
}

object SummationTrad {
  def summ(x: Int, y: Int):Int = {
    var sum = 0
    var i = x
    while(i <= y){
      sum += i
      i += 1
    }
    sum
  }

  def summR(x: Int, y: Int): Int = if(x > y) 0 else x + summR(x+1, y)

  private def cubes(x: Int): Int = x*x*x
  val cubes1 = (x: Int) => x*x*x
  def summCubes(x: Int, y: Int): Int = if(x > y) 0 else cubes(x) + summR(x+1, y)

  private def fact(x: Int): Int = if(x == 0) 1 else x*fact(x-1)

  def summFact(x: Int, y: Int): Int = if(x > y) 0 else fact(x) + summR(x+1, y)

  def summHOF(x: Int, y: Int, f: Int => Int): Int =  if(x > y) 0 else f(x) + summHOF(x+1, y, f)
  def productHOF(x: Int, y: Int, f: Int => Int): Int =  if(x > y) 1 else f(x) * productHOF(x+1, y, f)

  summHOF(1, 10, cubes)
  summHOF(1, 10, fact)

  def fib(n: Int): Int = {
    @tailrec
    def _fib(n: Int, fst: Int, snd: Int): Int = {
      if(n == 0) fst else if(n == 1) snd else _fib(n-1, snd, fst+snd)
    }
    _fib(n, 0, 1)
  }

}

object Combinators{
  def mapInt(lst: List[Int], f : Int => Int): List[Int] = {
    val buf = mutable.Buffer.empty[Int]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    // another way to write loop : Higher level functionality
    /*for(x <- lst){
      buf.append(x)
    }*/
    buf.toList
  }

  val res1 = mapInt(List(1,2,3,4), x => x * x)
  //Could also be written like :
  //val res1 = mapInt((1 to 4).toList, x => x * x)

  def mapGen[A,B](lst: List[A], f : A => B): List[B] = {
    val buf = mutable.Buffer.empty[B]
    val it = lst.iterator
    while(it.hasNext){
      val a = it.next()
      buf.append(f(a))
    }
    /*for(x <- lst){
      buf.append(x)
    }*/
    buf.toList
  }

  val res2 = mapGen(List(1,2,3),{x:Int => x*2}) // Type is required

  def mapCurrGen[A, B](list: List[A])(f: A => B): List[B] = {
    val iterator = list.iterator
    val result = mutable.Buffer.empty[B]

    while (iterator.hasNext) {
      result.append(f(iterator.next()))
    }

    result.toList
  }

  val res3 = mapCurrGen(List(1,2,3))({x => x*2}) // Type is not required
  // Can also be written like this :
  val res33 = mapCurrGen(List(1,2,3))(_*2)


  def map3[A, B](list: List[A])(f: A => B): List[B] = {
    var lst = List.empty[B]
    for(ele <- list){
      lst = f(ele) :: lst
    }
    lst.reverse
  }

  def map4[A, B](list: List[A])(f: A => B): List[B] = {
    if(list.isEmpty) List.empty[B]
    else {
      f(list.head) :: map4(list.tail)(f)
    }
  }

}





/*
{

  // def vs val
  //1. Check new function creation everytime
  val even: Int => Boolean = _ % 2 == 0 // even eq even return true
  //def even: Int => Boolean = _ % 2 == 0 // even eq even return false

  //2. Check evaluates when define
  val evenPrint: Int => Boolean = {println("Evaluating.."); _ % 2 == 0}
  //def evenPrint: Int => Boolean = {println("Evaluating.."); _ % 2 == 0}

  /*
  * Call By Value and call by name
  *
  * */
  def something() = {
    println("calling something")
    1 // return value
  }
  def callByValue(x: Int) = {
    println("x1=" + x)
    println("x2=" + x)
  }

  //callByValue(something())

  def callByName(x: => Int) = {
    println("x1=" + x)
    println("x2=" + x)
  }




  /*PritnMessage function
* */
  val myUpperCase: String =>String = str => {
    println(str)
    str toUpperCase
  }

  // Without Currying
  def printWelcomeMsg1(str1: String, str2: String) = println(myUpperCase(str1)+" "+str2)

  val withHello1: (String) => Unit = printWelcomeMsg1("hello", _)  //val withHello1(str:String) = printWelcomeMsg1("hello", str)
  withHello1("Akshay1")
  withHello1("Akshay1")

  // Curry Style 1
  def printWelcomeMsg2(str1: String)(str2: String) = println(myUpperCase(str1)+" "+str2)

  val withHello2: (String) => Unit = printWelcomeMsg2("hello")

  withHello1("Akshay2")
  withHello1("Akshay2")

  // Currying Style 2
  // Notice that we are also making use of Closure feature
  lazy val printWelcomeMsg3: (String) => (String) => Unit = (str:String) => {
    val upper= myUpperCase(str) // Could be Heavy computation
    (d:String) => println(upper+" "+ d)
  }

  val withHello3: (String) => Unit = printWelcomeMsg3("hello")

  withHello3("Akshay3")
  withHello3("Akshay3")
}
*/

