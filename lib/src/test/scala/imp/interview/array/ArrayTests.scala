package imp.interview.array

import org.junit.Assert
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ArrayTests extends AnyFunSuite {
  test("subArray") {
    def subArraySum = new SubArraySum()
    val nums = Array(-1,1,-1,1,1,2,-1,3,4,5,-5,2,-4,-5)

    val res_mutable_quad = subArraySum.solutionMutable_Quadratic(nums,1)
    Assert.assertEquals(8,res_mutable_quad)

    val res_immutable_quad = subArraySum.solutionImmutable_Quadratic(nums,1)
    Assert.assertEquals(8,res_immutable_quad)

    val res_immutable_linear = subArraySum.solutionImmutable_Linear(nums,1)
    Assert.assertEquals(8,res_immutable_linear)
  }
}