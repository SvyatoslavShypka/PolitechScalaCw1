object Main {

  //Zad.1
  private def flatten1[A](list1: List[List[A]]): List[A] = {

    if (list1 != null && list1.nonEmpty) {
      list1.head ++ flatten1(list1.tail)
    } else Nil
  }

  //Zad.2
  private def count[A](x: A, list: List[A]): Int = {
    if (list != null && list.nonEmpty) {
      val helpcount = if (x == list.head) 1 else 0
      helpcount + count(x, list.tail)
    } else 0
  }

  //Zad.3
  private def replicate[A](x: A, num: Int): List[A] = {
    if (num > 0) {
      List(x) ++ replicate(x, num - 1)
    } else Nil
  }

  //Zad.4
  private def sqrList(list: List[Int]): List[Int] = {
    if (list != null && list.nonEmpty) {
      List(list.head * list.head) ++ sqrList(list.tail)
    } else Nil
  }

  //Zad.5
  private def palindrome[A](list: List[A]): Boolean = {
    if (list == list.reverse) {true}
    else false
  }

  //Zad. 6
  private def listLength[A](list: List[A]): Int = {
    if (list != null && list.nonEmpty) {
      1 + listLength(list.tail)
    } else 0
  }

  //Zad. 7
  private def Equation(N: Double): Double = {
    val c = 1
    if (N == 1) 1
//    else (c * math.log(N)) / math.log(2) + Equation(N / 2)
    else (c * math.log10(N)) + Equation(N / 2)
  }

  def main(args: Array[String]): Unit = {
    println(flatten1(List(List(5, 6), List(1, 2, 3))))
    println(count("a", List("a", "l", "a")))
    println(replicate("la", 5))
    println(sqrList(List(1, 2, 3, 4, 5, 6)))
    println(palindrome(List("a", "l", "a")))
    println(palindrome(List(1,2,2,1)))
    println(palindrome(List("e","l","a")))
    println(listLength(List("a", "b", "c")))
    println(listLength(List()))
    println(Equation(1))
    println(Equation(2))
    println(Equation(4))
    println(Equation(8))
  }
}