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

  "Function flatten" should "flatten nested lists" in {
    assert(flatten(List()) === List())
    val result = List(1, 2, 3)
    assert(flatten(result) === result)
    assert(flatten(List(List(1, 2), 3)) === result)
    assert(flatten(List(List(1, List(2)), 3)) === result)
  }

  "Function compress" should "remove duplicates run together" in {
    assert(compress(List()) === List())
    val result = List(1, 2, 3)
    assert(compress(result) === result)
    assert(compress(List(1, 1, 2, 3)) === result)
    assert(compress(List(1, 2, 2, 3)) === result)
    assert(compress(List(1, 2, 3, 3)) === result)
    assert(compress(List(1, 1, 2, 2, 3, 3)) === result)
    assert(compress(List(1, 1, 2, 2, 1, 3, 3)) === List(1, 2, 1, 3))
  }

  "Function pack" should "wrap non-dupe elements in Lists" in {
    List(List(), List(1), List(2, 3, 4), List(2, 3, 2)).foreach(in_list =>
      assert(pack(in_list) === in_list.map(n => List(n)).toList)
    )
  }

  it should "list duplicates together" in {
    assert(pack(List(1, 1, 1)) === List(List(1, 1, 1)))
    assert(pack(List(1, 1, 2)) === List(List(1, 1), List(2)))
    assert(pack(List(1, 1, 2, 2)) === List(List(1, 1), List(2, 2)))
    assert(pack(List(1, 2, 2, 3)) === List(List(1), List(2, 2), List(3)))
    assert(pack(List(1, 2, 2, 2)) === List(List(1), List(2, 2, 2)))
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    	   === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a),
    		    List('d), List('e, 'e, 'e, 'e)))
  }
}
