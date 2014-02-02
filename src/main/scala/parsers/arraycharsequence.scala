package parsers

import scala.util
import java.util.Scanner
import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class ArrayCharSequence private (arrStart: Int, arrEnd: Int, arr: Array[Char]) extends CharSequence{
  val len = arrEnd - arrStart
  
  override def length() = len
  
  override def charAt(index: Int): Char = {
	if (index < 0)
	  throw new IllegalArgumentException()
    val pos = arrStart + index
    if (pos >= arrEnd)
      throw new IndexOutOfBoundsException()
    arr(pos) 
  }
  
  def apply(index: Int): Char = charAt(index)
  
  override def subSequence(start: Int, end: Int) = { 
    ArrayCharSequence(start + arrStart, end + arrStart, arr)
  }
  
  override def toString: String = arr.slice(arrStart, arrEnd).mkString
}

object ArrayCharSequence{
  def apply(arrStart: Int, arrEnd: Int, arr: Array[Char]): ArrayCharSequence = {
    val len = arr.length
    def failIfInvalid(index: Int) {
      if (!(index >= 0 && index <= len))
        throw new IndexOutOfBoundsException()
    }
    failIfInvalid(arrStart)
    failIfInvalid(arrEnd)
    if (arrStart > arrEnd)
      throw new IllegalArgumentException()
    
    new ArrayCharSequence(arrStart, arrEnd, arr)
  }
  
  def fromCharArray(arr: Array[Char]): ArrayCharSequence = ArrayCharSequence(0, arr.length, arr)

  def fromString(input: String) = fromCharArray(input.toCharArray())
}