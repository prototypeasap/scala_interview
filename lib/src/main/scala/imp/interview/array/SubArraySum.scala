package imp.interview.array

import scala.annotation.tailrec

/*
  Problem:
  Given an array of integers nums and an integer k,
  return the total number of continuous subarrays whose sum equals to k.

  Author: Ivan Malyuta; 01/17/2022
 */
class SubArraySum {
  def someLibraryMethod(): Boolean = true

  // Brute force O(n^2) solution using mutated rolling sum variable
  def solutionMutable_Quadratic(nums: Array[Int], k: Int): Int = {
    var res = 0;

    for (i <- Range(0, nums.length)) {
      var sum = 0;
      for (j <- Range(i, nums.length)) {
        sum += nums(j);
        if (sum == k)
          res = res + 1;
      }
    }
    res;
  }

  // Brute force O(n^2) solution using recursion (immutable)
  def solutionImmutable_Quadratic(nums: Array[Int], k: Int): Int = {
    @tailrec
    def solution(nums: List[Int], k: Int, a: Int): Int = nums match {
      case Nil => a
      case _ => solution(nums.tail, k, count(nums, 0, k, 0) + a)
    }

    @tailrec
    def count(nums: List[Int], rsum: Int, k: Int, a: Int): Int = nums match {
      case Nil => a
      case head :: tail => {
        val sum = rsum + head
        count(tail, sum, k, a + (if (sum == k) 1 else 0))
      }
    }

    solution(nums.toList, k, 0)
  }

  // Linear solution using auxiliary hashmap
  //  Map stores number of occurrences for each rolling sum.
  def solutionImmutable_Linear(nums: Array[Int], k: Int): Int = {
    @tailrec
    def solution(nums: List[Int], k: Int, am: Map[Int,Int], rs: Int, res: Int): Int = nums match {
      case Nil => res
      case head::tail => {
        val nrs = rs + head
        val nres = res + am.getOrElse(nrs-k,0)
        val nam = am + (nrs -> (am.getOrElse(nrs,0)+1))
        solution(tail, k, nam, nrs, nres)
      }
    }

    solution(nums.toList,k,Map(0 -> 1),0,0)
  }
}
