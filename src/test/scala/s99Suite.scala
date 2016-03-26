/*
   Copyright Ⓒ 2013 Brett Smith

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

import org.scalatest._

class S99Spec extends FlatSpec {
  import S99._

  "Function last" should "return the last element of a list" in {
    1 to 10 foreach(max => assert(last((1 to max).toList) === Some(max)))
  }

  it should "return None for an empty list" in {
    assert(last(List()) === None)
  }

  "Function penultimate" should "return the second-last element of a list" in {
    2 to 10 foreach(
      max => assert(penultimate((1 to max).toList) === Some(max - 1))
    )
  }

  it should "return None for a too-short list" in {
    assert(penultimate(List(1)) === None)
  }

  "Function nth" should "return the corresponding index" in {
    val l = (0 to 5).map(n => n * n).toList
    (0 to 5).foreach(n => assert(nth(n, l) === Some(n * n)))
  }

  it should "return None past the end of the list" in {
    assert(nth(1, List()) === None)
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

  "Function slice" should "return sublists" in {
    val list = 1.to(5).toList
    assert(slice(0, 3, list) === List(1, 2, 3))
    assert(slice(1, 4, list) === List(2, 3, 4))
    assert(slice(2, 5, list) === List(3, 4, 5))
  }

  it should "support negative indices" in {
    val list = 1.to(5).toList
    assert(slice(-2, 5, list) === List(4, 5))
    assert(slice(-3, 5, list) === List(3, 4, 5))
    assert(slice(1, -1, list) === List(2, 3, 4))
  }

  it should "return Nil out of bounds" in {
    assert(slice(0, 1, Nil) === Nil)
    assert(slice(1, 2, List(1)) === Nil)
    assert(slice(0, -2, List(1)) === Nil)
  }

  "Function rotate" should "rotate the given list" in {
    val list = (1 to 5).toList
    assert(rotate(0, list) === list)
    assert(rotate(1, list) === List(2, 3, 4, 5, 1))
    assert(rotate(2, list) === List(3, 4, 5, 1, 2))
    assert(rotate(3, list) === List(4, 5, 1, 2, 3))
    assert(rotate(4, list) === List(5, 1, 2, 3, 4))
    assert(rotate(5, list) === List(1, 2, 3, 4, 5))
  }

  "Function removeAt" should "remove the nth element of the list" in {
    val list = (1 to 5).toList
    assert(removeAt(0, list) === List(2, 3, 4, 5))
    assert(removeAt(1, list) === List(1, 3, 4, 5))
    assert(removeAt(2, list) === List(1, 2, 4, 5))
    assert(removeAt(3, list) === List(1, 2, 3, 5))
    assert(removeAt(4, list) === List(1, 2, 3, 4))
    assert(removeAt(5, list) === list)
  }
}
