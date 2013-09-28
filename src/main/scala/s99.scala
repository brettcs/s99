import java.lang.Math.abs

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
    case head :: tail => head :: flatten(tail)
  }

  def compress[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: y :: tail if x == y => compress(y :: tail)
    case x :: tail => x :: compress(tail)
  }

  def pack[A](list: List[A], head_tally: Int = 0): List[List[A]] = list match {
    case Nil => Nil
    case x :: y :: tail if x == y => pack(y :: tail, head_tally + 1)
    case head :: tail =>
      (0.to(head_tally).map(x => head)).toList :: pack(tail, 0)
  }

  def slice[A](start: Int, stop: Int, list: List[A]): List[A] = {
    val length = list.length
    if ((abs(start) > length) || (abs(stop) > length))
      throw new IndexOutOfBoundsException
    val i_start = (length + start) % length
    val i_stop = (length + stop - 1) % length
    (for (index <- i_start to i_stop) yield list(index)).toList
  }

  def rotate[A](count: Int, list: List[A]): List[A] = {
    if (list == Nil)
      Nil
    else if ((count % list.length) == 0)
      list
    else
      slice(count, 0, list) ::: slice(0, count, list)
  }
}
