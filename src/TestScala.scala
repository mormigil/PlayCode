import scala.annotation.tailrec
import scala.collection.mutable.Set
import scala.collection.mutable.Seq
import scala.collection.mutable.ListBuffer
import scala.collection.concurrent.TrieMap

object TestScala {
  def main(args: Array[String]): Unit = {
    val test = Array(4, 5, 6, 7, 0, 1, 2)
    val start = test(0)
    val target = 2
    val pivot = findPivot(test, 0, test.length-1, start)
    
    if(target>=start){
      println(binSearch(test, start, pivot-1, target))
    } else {
      println(binSearch(test, pivot, test.length-1, target))
    }
    val test2 = Array(5, 7, 7, 8, 8, 10)
    println(findTargetRange(test2, 0, test.length-1, true, 8))
    println(findTargetRange(test2, 0, test.length-1, false, 8))
    
    val palindromePartition = "appalachia"
    
    val list = test2.toList.map(x=>(x/3, x%3, x%4, x%2))
    list.groupBy(_._1).filter(x=>x._2.size==3).size>0
    list.groupBy(_._2).filter(x=>x._2.size==3).size>0
    list.groupBy(_._3).filter(x=>x._2.size==3).size>0
    list.groupBy(_._4).filter(x=>x._2.filter(y=>y!=0&&y!=8).size==3).size>0
    
    val guess = for (x <- ListBuffer.range(0, 3); y <- ListBuffer.range(0, 3)) yield (x, y)
    guess.remove(5)
    
    palSplitter(palindromePartition.toCharArray(), 0, Array(), Array()).map(x=>x.map(y=>y.mkString).mkString(" ")).foreach(x=>println(x))
    //.map(x=>x.map(y=>y.toString()).mkString(" ")).foreach(x=>println(x))
    longestSequence()
    largestNum()
    
    val majorityNums = Seq(3, 4, 5, 6, 3, 8, 7, 10, 3)
    val grid = Seq(Seq(1, 0, 1, 0, 0), Seq(1, 0, 1, 1, 1), Seq(1, 1, 1, 1, 1), Seq(1, 0, 1, 1, 1))
    println(findMaxSquare(grid, 0, 0, Seq(Seq()), 0))
    
    println(sumRange(0, 2))
  }
  
  def maxProduct(numList: List[Int]): Int = numList match {
    case Nil => 0
  }
  
  def twoSum(j: Int): Array[Int] = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val uniqueSet = nums.map(x=>(x-j, 1)).groupBy(_._1).map{case(group, traversable) => (group, traversable.map(_._2).sum)}
    nums.zipWithIndex.filter(x=>uniqueSet(x._1)>1).map(x=>x._2)
  }
  
  def sumRange(i: Int, j: Int): Int = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    nums.slice(i, j+1).sum
  }
  
  def findMaxSquare(grid: Seq[Seq[Int]], v1: Int, h1: Int, areas: Seq[Seq[Int]], max: Int): Int = {
    println(areas)
    var nextMax = max
    if(grid(v1)(h1)==0)
      areas(v1) = areas(v1).:+(0)
    else {
      val left = if(h1>0) areas(v1)(h1-1) else 0
      val diag = if(h1>0&&v1>0) areas(v1-1)(h1-1) else 0
      val up = if(v1>0) areas(v1-1)(h1) else 0
      val area = left.min(diag).min(up) + 1
      nextMax = max.max(area)
      areas(v1) = areas(v1).:+(area)
    }
    if(h1<grid(0).length-1){
      findMaxSquare(grid, v1, h1+1, areas, nextMax)
    } else if (v1<grid.length-1){
      findMaxSquare(grid, v1+1, 0, areas.:+(Seq()), nextMax)
    } else {
      nextMax
    }
  }
  
  def largestNum(): Unit = {
    val nums = Seq(3, 30 , 34, 5, 9)
    val largestTrie = Seq()
    println(nums.map(x=>x.toString().map(y=>y.asDigit)).sortBy(x=>x.toIterable).reverse)
  }
  
  def addToTrie(trie: Seq[Any], elem: Seq[Integer], pointer: Integer): Any = {
    if(pointer+1==elem.length){
      trie(elem(pointer)) = elem(pointer)
    }
    trie(elem(pointer)) = addToTrie(trie, elem, pointer+1)
    trie(0)
  }
  
  def longestSequence(): Unit = {
    val nums = List(100, 4, 200, 1, 3, 2)
    val allRanges = Set[Integer]()
    val startNums = Set[Integer]()
    nums.foreach{x =>
      allRanges += x
      if(!allRanges(x-1)){
        startNums += x
      }
      if(allRanges(x+1)){
        startNums -= (x+1)
      }
    }
    
    println(startNums.map(x=>rangeLength(allRanges, x, x)).max)
  }
  
  @tailrec
  def rangeLength(allRanges: Set[Integer], x: Integer, nextVal: Integer): Integer = {
    println(nextVal)
    if(!allRanges(nextVal)){
      nextVal-x
    } else {
      rangeLength(allRanges, x, nextVal+1)
    }
  }
  
//  @tailrec
  def palSplitter(str: Array[Char], pointer: Int, curStr: Array[Array[Char]], listResults: Array[Array[Array[Char]]]): Array[Array[Array[Char]]] = {
//    println(curStr.map(x=>x.mkString).mkString(" "))
    if(pointer==str.length){
      return listResults.:+(curStr)
    } else{ 
      if (curStr.lastOption.isDefined && curStr.last.head==str(pointer)){
        val nextStr = curStr.clone()
        nextStr.update(curStr.length-1, curStr.last.:+(str(pointer)))
        val result1 = listResults ++ palSplitter(str, pointer+1, nextStr, listResults)
      }
      
      if (curStr.isDefinedAt(curStr.length-2)){
        val secondLast = curStr(curStr.length-2)
        if(secondLast.length==1&&secondLast.head==str(pointer)){
          val sepStr = curStr.splitAt(curStr.length-2)
          listResults ++ palSplitter(str, pointer+1, sepStr._1.:+(sepStr._2.:+(Array(str(pointer))).flatMap(x=>x)), listResults)
        }
      }
      
      listResults ++ palSplitter(str, pointer+1, curStr.:+(Array(str(pointer))), listResults)
    }
  }
  
  @tailrec
  def findPivot(nums: Array[Int], start: Int, end: Int, realStart: Int): Int = {
    val mid = ((start+end)/2)
    val prior = mid-1
    if(nums(prior)>nums(mid)){
      mid
    } else if(end<=start){
      return 0
    } else if(realStart<nums(mid)){
      findPivot(nums, mid, end, realStart)
    } else{
      findPivot(nums, start, mid, realStart)
    }
  }
  
  @tailrec
  def findTargetRange(nums: Array[Int], start: Int, end: Int, first: Boolean, target: Int): Int = {
    val mid = ((start+end)/2)
    if(first && (mid==0||nums(mid-1)<target) && nums(mid)==target){
      mid
    } else if(!first && (mid+1==nums.length||nums(mid+1)>target) && nums(mid)==target){
      mid
    } else if (target==nums(mid)){
      if(first){
        findTargetRange(nums, start, mid, first, target)
      } else {
        findTargetRange(nums, mid+1, start, first, target)
      }
    } else if (target < nums(mid)){
      findTargetRange(nums, start, mid, first, target)
    } else {
      findTargetRange(nums, mid+1, start, first, target)
    }
  }
//  def recursiveFindMedian(nums1: Array[Integer], nums2: Array[Integer], s1: Integer, s2: Integer, e1: Integer, e2: Integer){
//    val medianPos = (nums1.length+nums2.length)/2
//    val m1 = (s1+e1)/2
//    val m2 = (s2+e2)/2
//    if(m1+m2==medianPos && 
//  }
  
  @tailrec
  def binSearch(nums: Array[Int], start: Int, end: Int, target: Int): Int = {
    val mid = ((start+end)/2)
    if(nums(mid)==target){
      mid
    } else if(end<=start){
      return -1
    }else if(nums(mid)<target){
      binSearch(nums, mid+1, end, target)
    } else{
      binSearch(nums, start, mid, target)
    }
  }
  
//  @tailrec
  def maxProductRecurse(nums: List[Int], lastProduct: Int, greatest: Tuple2[Int, Int]): Int = {
      nums match {
          case Nil => lastProduct
          case x :: xs => Math.max(maxProductRecurse(xs, lastProduct*x, greatest), maxProductRecurse(xs, x, greatest))
      }
  }
}