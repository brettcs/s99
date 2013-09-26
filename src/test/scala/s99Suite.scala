import org.scalatest._

class S99Spec extends FlatSpec {
  import S99._

  "Function last" should "return the last element of a list" in {
    1 to 10 foreach(max => assert(last((1 to max).toList) === max))
  }

  "Function penultimate" should "return the second-last element of a list" in {
    2 to 10 foreach(max => assert(penultimate((1 to max).toList) === (max - 1)))
  }

  it should "throw an exception for a too-short list" in {
    intercept[NoSuchElementException] {
      penultimate(List(1))
    }
  }

  "Function nth" should "return the corresponding index" in {
    val l = (0 to 5).map(n => n * n).toList
    (0 to 5).foreach(n => assert(nth(n, l) === (n * n)))
  }

  it should "raise an exception past the end of the list" in {
    intercept[NoSuchElementException] {
      nth(1, List())
    }
  }

  "Function length" should "return the length of the list" in {
    (0 to 5).foreach(end => assert(length((1 to end).toList) === end))
  }

  "Function reverse" should "reverse a list" in {
    (0 to 5).foreach(
      end => assert(reverse((1 to end).toList) === end.to(1, -1).toList)
    )
  }

  "Function isPalindrome" should "find palindromes" in {
    assert(isPalindrome(List()))
    assert(isPalindrome(List(2)))
    assert(isPalindrome(List(2, 5, 2)))
    assert(isPalindrome(List(2, 5, 5, 2)))
    assert(isPalindrome(List(2, 5, 9, 5, 2)))
    assert(!isPalindrome(List(2, 5, 9, 4, 2)))
    assert(!isPalindrome(List(2, 5, 4, 2)))
    assert(!isPalindrome(List(2, 3)))
  }
}
