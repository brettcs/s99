object S99 {
  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => x
      case _ :: tail => last(tail)
    }
  }
}
