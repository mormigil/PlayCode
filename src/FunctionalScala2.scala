import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

object FunctionalScala2 {
  
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])
  
  def main(args: Array[String]): Unit = {
    val leaf15 = TreeNode(15, None, None)
    val leaf7 = TreeNode(7, None, None)
    val leaf9 = TreeNode(9, None, None)
    val parent20 = TreeNode(20, Option(leaf15), Option(leaf7))
    val root = TreeNode(3, Option(leaf9), Option(parent20))
    val test = List(3, 4, 2, 3)
    println(NumOutOfOrder(test, 0, Int.MinValue))
//    println(zigzagLevelOrder(root))
    val board: Array[Array[Char]] = Array()
    val rowColBoard = board.toList.flatMap(_.toList).zipWithIndex.map{case(cVal, index) => (index/9, index%9, cVal)}
    val rows = rowColBoard.groupBy(x=>x._1) ++ rowColBoard.groupBy(x=>x._2) ++ rowColBoard.groupBy(x=>(x._1/3, x._2/3))
    val invalidList = rows.map(x=>x._2.map(y=>y._3)).filter(x=>x.distinct.size==x.size)
    invalidList.size>0
    val matrix: Array[Array[Int]] = Array()
    val listWithIndex = for (list <- matrix.toList.zipWithIndex; x <- list._1.toList.zipWithIndex) yield ((list._2, x._2), x._1)
    val mapOfIndex = listWithIndex.toMap
    matrix.toList.zipWithIndex.flatMap(x=>x._1.toList.zipWithIndex.map(y=>(x._2, y._2, y._1))).groupBy(x=>(x._1, x._2))
  }
  
//  def spiralIndex(x: Int, y: Int, dir: Boolean, xLBound: Int, xUBound: Int, 
//      yLBound: Int, yUBound: Int, indexes: List[(Int, Int)]): List[(Int, Int)] = {
//    if(dir && x<=xUBound){
//      
//    }
//  }
  
  def ordered(x: Int, y: Int, lastMax: Int): Int = {
    println(x, y)
    if(x>y || lastMax>y) 1 else 0
  }
  
  def findPairs(nums: Array[Int], k: Int): Int = {
    val combinedList = nums.distinct.toList.map(x=>x+k) ++ nums.distinct
//    combinedList.aggregate(Map[Int, Int]())((x, y)=> x + (y, 1), (x, y)=> x + ((x, x.getOrElse(x, 0) + 1)))
    combinedList.groupBy(x=>x).filter(x=>x._2.size>1).size
  }
  
  @tailrec
  def NumOutOfOrder(nums: List[Int], outOfOrder: Int, lastMax: Int): Int = nums match{
    case Nil => outOfOrder
    case x :: xs => NumOutOfOrder(xs, outOfOrder + ordered(x, xs.headOption.getOrElse(Int.MaxValue), lastMax), x)
  }
  
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    @tailrec
    def nextLevel(treenodes: List[TreeNode], curList: List[List[Int]]): List[List[Int]] = treenodes match {
        case Nil => curList
        case _ => {
            val allChildren = treenodes.flatMap(x=>List(x.left, x.right)).flatMap(x=>x)
            val levelValues = allChildren.map(x=>x.value)
            nextLevel(allChildren, curList :+ levelValues)
        }
    }
    val bfsVals = nextLevel(List(root), List(List(root.value))).filter(x=> !x.isEmpty).zipWithIndex
    def isOdd(i: Int) = i%2 == 1
    bfsVals.map{case(values, number)=> if(isOdd(number)) values.reverse else values}
  }
}