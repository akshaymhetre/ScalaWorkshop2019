package com.learning.workshop.s01e03

object Assign extends App {

  def Cache[A,B](f:A=>B) = new Function[A,B] {
    import scala.collection.mutable.Map
    private val cache = Map.empty[A, B]
    override def apply(v:A):B = cache getOrElseUpdate(v,f(v))
  }

  val incrByOne = Cache (
    (a: Int) => {
      println("Executed")
      a + 1
    }
  )

  println(incrByOne(1))
  println(incrByOne(1))

//  val incrByOneWithCached = Cache( (a:Int) => {
//    println("Executing function")
//    a+1
//  })
//  println(incrByOneWithCached(1))
//  println(incrByOneWithCached(1))

}


object assign3 {
  object Answer{
    sealed trait Day
    case object Weekday extends Day
    case object Weekend extends Day

    sealed trait Customer
    case object RegularCustomer extends Customer
    case object RewardsCustomer extends Customer

    case class Category(customer: Customer, day: Day)

    case class Request(customer: Customer, days: List[Day]){
      def categories : List[Category] =
        days.map(d => Category(customer, d))
    }

    case class Hotel(name: String, rating: Int,
                     rateCard: Map[Category, Int]){
      def costOf(request: Request): Int =
        request.categories.map(rateCard).sum
    }

    class HotelService(hotels: Hotel*){
      def bestHotel(request: Request): Hotel =
        hotels.minBy(h => (h.costOf(request), -h.rating))
    }
  }

}


object MemoizeExample {

  def Cache[A,B](f:A=>B) = new Function[A,B] {
    import scala.collection.mutable.Map
    private val cache = Map.empty[A, B]
    override def apply(v:A):B = cache getOrElseUpdate(v,f(v))
  }

  val incrByOneWithCached = Cache( (a:Int) => a + 1 )

  lazy val nth_fib: Int => Int = Cache { (num:Int) =>
    num match {
      case 1 => 0
      case 2 => 1
      case n => nth_fib(n - 1) + nth_fib(n - 2)
    }
  }

  lazy val nth_fib1: Int => Int = Cache {
    case 1 => 0
    case 2 => 1
    case n => nth_fib1(n-1) + nth_fib1(n-2)
  }

  /**
    * Subset sum algorithm - Can you achieve sum n using elements from list?
    **/


  val isSubsetSumAchievable :(((List[Int], Int)) => Boolean) =  Cache {
    case (_, 0) => true         // 0 can always be achieved using empty list
    case (Nil, _) => false      // we can never achieve non-zero if we have empty list
    case (a :: as, x) => isSubsetSumAchievable(as, x - a) || isSubsetSumAchievable(as, x)      // try with/without a.head
  }
  isSubsetSumAchievable(List(1,4,19,7,23,15),12)
  isSubsetSumAchievable(List(1,4,19,7,23,15),19)
  isSubsetSumAchievable(List(1,4,19,7,23,15),25)

}
object MemoizeExample1 {
  def memoize[I, O](f: I => O) = new scala.collection.mutable.HashMap[I, O]() {
    println("Hello")
    override def apply(key: I): O = {
      //val obj = getOrElse(key, s"$key not found")
      //println(obj)
      getOrElseUpdate(key, f(key))
    }
  }

  val multiplyByTwoWithCached = memoize { (num:Int) =>
    num*2
  }
}

object MemoizeExample2 {
  class Memoize1[-T, +R](f: T => R) extends (T => R) {
    import scala.collection.mutable
    private[this] val vals = mutable.Map.empty[T, R]

    override def apply(x: T): R = {
      if (vals.contains(x)) {
        vals(x)
      }
      else {
        val y = f(x)
        vals += ((x, y))
        y
      }
    }
  }

  object Memoize1 {
    def apply[T, R](f: T => R) = new Memoize1(f)
  }

  def strSqLen(s: String) = s.length*s.length

  val strSqLenMemoized = Memoize1(strSqLen)
  val a = strSqLenMemoized("hello Memo")
  val b = strSqLen("hello Memo")
  assert(a == b)
}
