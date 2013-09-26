object S99 {
  def last[A](list: List[A]): A = {
    list match {
      case Nil => throw new IllegalArgumentException
      case x :: Nil => x
      case x => last(list.tail)
    }
  }
}
