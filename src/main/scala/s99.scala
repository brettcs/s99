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
}
