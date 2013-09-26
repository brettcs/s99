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
}
