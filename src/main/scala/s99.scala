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

import java.lang.Math.abs

object S99 {
  def last[A](list: List[A]): Option[A] = {
    list match {
      case Nil => None
      case x :: Nil => Some(x)
      case _ :: tail => last(tail)
    }
  }

  def penultimate[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case _ :: Nil => None
    case x :: _ :: Nil => Some(x)
    case _ :: tail => penultimate(tail)
  }

  def nth[A](index: Int, list: List[A]): Option[A] = (index, list) match {
    case (_, Nil) => None
    case (0, x :: _) => Some(x)
    case (i, _ :: tail) => nth(i - 1, tail)
  }

  def length(list: List[Any], count: Int = 0): Int = list match {
    case Nil => count
    case _ :: tail => length(tail, count + 1)
  }

  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case head :: tail => reverse(tail) ++ List(head)
  }

  def isPalindrome[A](list: List[A]): Boolean =
    list.iterator.zip(list.reverseIterator).forall { case (a, b) => a == b }

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case (head: List[Any]) :: tail => flatten(head) ::: flatten(tail)
    case head :: tail => head :: flatten(tail)
  }

  def compress[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: y :: tail if x == y => compress(y :: tail)
    case x :: tail => x :: compress(tail)
  }

  def pack[A](list: List[A], head_tally: Int = 0): List[List[A]] = list match {
    case Nil => Nil
    case x :: y :: tail if x == y => pack(y :: tail, head_tally + 1)
    case head :: tail =>
      (0.to(head_tally).map(x => head)).toList :: pack(tail, 0)
  }

  def slice[A](start: Int, stop: Int, list: List[A]): List[A] = {
    val length = list.length
    def boundIndex(i: Int): Int =
      if (i < 0)
        (length + i).max(0)
      else
        i
    val boundStart = boundIndex(start)
    list.drop(boundStart).take(boundIndex(stop) - boundStart)
  }

  def rotate[A](count: Int, list: List[A]): List[A] = {
    val length = list.length
    slice(count, length, list) ::: slice(0, count, list)
  }

  def removeAt(index: Int, list: List[Any]): List[Any] = (index, list) match {
    case (_, Nil) => Nil
    case (0, _ :: tail) => tail
    case (n, x :: tail) => x :: removeAt(n - 1, tail)
  }
}
