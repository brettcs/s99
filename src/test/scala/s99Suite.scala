import org.scalatest._

class S99Spec extends FlatSpec {
  import S99._

  "Function last" should "return the last element of a list" in {
    1 to 10 foreach(max => assert(last((1 to max).toList) === max))
  }
}
