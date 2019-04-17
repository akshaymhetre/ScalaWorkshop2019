package com.learning.workshop.s01e02

object Practice extends App {
  val isEven = (n: Int) => n%2 == 0
  val isEven1: Int => Boolean = _%2 == 0

  val addition: (Int, Int) => Int = (a:Int, b: Int) => a + b
  val addition1: (Int, Int) => Int  = (a, b) => a + b
  val addition2: (Int, Int) => Int  = _ + _



  def summHOF(x: Int, y: Int, f: Int => Int): Int =  if(x > y) 0 else f(x) + summHOF(x+1, y, f)
  def productHOF(x: Int, y: Int, f: Int => Int): Int =  if(x > y) 1 else f(x) * productHOF(x+1, y, f)


  def mapReduce(x: Int, y: Int,
                map: Int => Int,
                reduce: (Int, Int) => Int,
                pivot: Int): Int =

    if(x > y) pivot else reduce(map(x), mapReduce(x+1, y, map, reduce, pivot))

  mapReduce(1, 10, (a: Int) => a*a, _+_ , 0)
  mapReduce(1, 10, (a: Int) => a*a, _*_ , 1)

  val myUpperCase: String =>String = str => {
    println("Inside myUpperCase")
    str toUpperCase
  }

  def printWelcomeMsg(str1: String, str2: String) {
    println(myUpperCase(str1)+" "+str2)
  }

  def helloWelcomeMsg(str: String) = printWelcomeMsg("Hello", str)

//  helloWelcomeMsg("Akshay1")
//  helloWelcomeMsg("Akshay2")

  def add(a: Int, b: Int) = a+b

  def add1(a: Int)(b: Int) = a+b
  def add2(a: Int)(b: Int)(c: Int) = a+b+c

  def add2 = (a: Int) => ((b: Int) => a+b)
  def add3 = (a: Int) => (b: Int) => a+b

  val incrByOne: Int => Int = add3(1)

  incrByOne(2)



  def add4: Int => (Int => (Int => Int)) = (a: Int) => (b: Int) => (c: Int)=> a+b+c


  val add5: Int => (Int => Int) = add4(1)

  def printWelcomeMsg1(str1: String)(str2: String) {
    println(myUpperCase(str1)+" "+str2)
  }

  val helloWelcomeMsg1 = printWelcomeMsg1("Hello")(_)

  //helloWelcomeMsg1("Akshay1")
  //helloWelcomeMsg1("Akshay2")


  val printWelcomeMsg2 = (prefix: String) => {
    val upperPrefix = myUpperCase(prefix)
    (suffix: String) => println(upperPrefix + " " + suffix)
  }

  val helloWelcomeMsg2: String => Unit = printWelcomeMsg2("Hello")
  helloWelcomeMsg2("Akshay1")
  helloWelcomeMsg2("Akshay2")

}
