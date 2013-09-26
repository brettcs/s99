object S99 {
  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => x
      case _ :: tail => last(tail)
    }
  }

  def penultimate[A](list: List[A]): A = list match {
    case Nil => throw new NoSuchElementException
    case _ :: Nil => throw new NoSuchElementException
    case x :: _ :: Nil => x
    case _ :: tail => penultimate(tail)
  }

  def nth[A](index: Int, list: List[A]):A = (index, list) match {
    case (_, Nil) => throw new NoSuchElementException
    case (0, x :: _) => x
    case (i, _ :: tail) => nth(i - 1, tail)
  }

  def length(list: List[Any], count: Int = 0): Int = list match {
    case Nil => count
    case _ :: tail => length(tail, count + 1)
  }

  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case head :: tail => reverse(tail) ++ List(head)
  }

  def isPalindrome(list: List[Any]): Boolean = {
    val end = list.length - 1
    !0.to(end).map(i => list(i) == list(end - i)).contains(false)
  }

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => List(head) ::: flatten(tail)
  }
}
