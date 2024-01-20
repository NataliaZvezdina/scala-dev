package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps
import scala.util.control.TailCalls.{TailRec, done}



/**
 * referential transparency
 */


 object referential_transparency{

  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int =
    if(n == 0) 1
    else n * factRec(n - 1)




  def tailRec(n: Int): Int = {
    @tailrec
    def loop(i: Int, accum: Int): Int =
      if(i == 0) accum else loop(i - 1, n * accum)
    loop(n, 1)
  }




  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */
  def fib(n: Int): Int = {

    def loop(n: Int): TailRec[Int] = n match {
      case n if n <= 2 => done(1)
      case n =>
        for {
          r <- loop(n - 1)
          l <- loop(n - 2)
        } yield r + l
    }
    loop(n).result
  }
  println(fib(5))

}

object hof{


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }

  def doomy(str: String) = {
    Thread.sleep(1000)
    println(str)
  }

  val r1: String => Unit = logRunningTime(doomy)

  r1("Hello")



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  val isEven: Int => Boolean = not(isOdd)

  println(isOdd(3))
  println(isEven(2))

  var i = 1

  def incr(f: Int => Int): Int => Int =
    a => {
      i = 5
      f(i + 2)
    }

  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def partial2[A, B, C](a: A, f: (A, B) => C): B => C =
    f.curried(a)

  def sum(x: Int, y: Int): Int = x + y

  val p: Int => Int = partial(2, sum)

  p(2) // 4
  p(3) // 5
  p(4) // 6
















  trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
   def createRequestSubscription() = ???



}






/**
 *  Реализуем тип Option
 */



 object opt {


  class Animal
  class Dog extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // 1. invariance
  // 2. covariance  A <- B  Option[A] <- Option[B]
  // 3. contravariance A <- B Option[A] -> Option[B]
  sealed trait Option[+T]{
    def isEmpty: Boolean = this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty option")
    }


    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def printIfAny(): Unit = this match {
      case Some(v) => println(v)
      case None =>
    }

    def zip[B](b: Option[B]): Option[(T, B)] = this match {
      case Some(v) if !b.isEmpty => Option((v, b.get))
      case _ => None
    }

    def filter(p: T => Boolean): Option[T] = this match {
      case Some(v) if p(v) => Option(v)
      case _ => None
    }

  }

  case class Some[V](v: V) extends Option[V]
  case object None extends Option[Nothing]   // Any <- Dog

  object Option{
    def apply[T](v: T): Option[T] =
      if(v == null) None
      else Some(v)
  }

  val o1: Option[Int] = Option(1)
  o1.isEmpty // false

  o1.printIfAny()

  val o2: Option[Nothing] = None
  o2.printIfAny()

  val o3 = Option("Str")

  val zipped1 = o1.zip(o3)
  val zipped2 = o1.zip(o2)
  println(s"Zipped values: $zipped1")
  println(s"Not zipped values: $zipped2")

  var filtered1: Option[Int] = o1.filter(_ > 0)
  println(filtered1)

  val o4: Option[Int] = None
  val filtered4: Option[Int] = o4.filter(_ > 100)
  println(filtered4)









  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

    trait List[+T]{
      def ::[TT >: T](elem: TT): List[TT] = List.::(elem, this)

      def mkString(sep: String): String = {
        @tailrec
        def loop(lst: List[T], acc: StringBuilder): StringBuilder = lst match {
          case List.Nil => acc
          case List.::(head, List.Nil) => loop(List.Nil, acc.append(head.toString))
          case List.::(head, tail) => loop(tail, acc.append(head.toString).append(sep))
        }

        loop(this, new StringBuilder()).toString()
      }

      def reverse(): List[T] = {
        @tailrec
        def loop(lst: List[T], acc: List[T]): List[T] = lst match {
          case List.Nil => acc
          case List.::(head, tail) => loop(tail, head :: acc)
        }

        loop(this, List.Nil)
      }

      def map[B](f: T => B): List[B] = {
        @tailrec
        def loop(lst: List[T], acc: List[B]): List[B] = lst match {
          case List.Nil => acc
          case List.::(head, tail) => loop(tail, f(head) :: acc)
        }

        loop(this, List.Nil).reverse()
      }

      def filter(p: T => Boolean): List[T] = {
        @tailrec
        def loop(lst: List[T], acc: List[T]): List[T] = lst match {
          case List.Nil => acc
          case List.::(head, tail) if p(head) => loop(tail, head :: acc)
          case List.::(_, tail) => loop(tail, acc)
        }

        loop(this, List.Nil).reverse()
      }

    }

    object List{
      case class ::[A](head: A, tail: List[A]) extends List[A]
      case object Nil extends List[Nothing]

      def apply[A](v: A*): List[A] =
        if(v.isEmpty) List.Nil else new ::(v.head, apply(v.tail:_*))

      def incList(lst: List[Int], num: Int = 1): List[Int] = lst.map(_ + num)

      def shoutString(lst: List[String], suffix: String = "!"): List[String] = lst.map(_ + suffix)
    }

    val l1: List[Int] = List(1, 2, 3)
    val l2: List[Int] = 1 :: 2 :: 3 :: List.Nil

    println(l1)
    println(l2)

    println(l1.mkString("-"))

    val l3: List[String] = List("foo", "bar", "baz")
    println(l3.mkString(", "))

    println(l3.reverse().mkString(", "))

    println(l1.map(_ * 10).mkString(", "))

    println(l1.filter(_ % 2 != 0).mkString(", "))

    println(List.incList(l1, 1000).mkString(", "))

    println(List.shoutString(l3, "!!!").mkString(", "))



   /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */

    /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */

    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */


    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

 }