import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

object FunctionalScala {
  
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])
  
  def main(args: Array[String]): Unit = {
    val leaf15 = TreeNode(15, None, None)
    val leaf7 = TreeNode(15, None, None)
    val leaf9 = TreeNode(15, None, None)
    val parent20 = TreeNode(20, Option(leaf15), Option(leaf7))
    val root = TreeNode(3, Option(leaf9), Option(parent20))
    zigzagLevelOrder(root)
  }
  
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    def nextLevel(treenodes: List[TreeNode], curList: List[List[Int]]): List[List[Int]] = treenodes match {
        case Nil => curList
        case _ => {
            val allChildren = treenodes.flatMap(x=>List(x.left, x.right)).flatMap(x=>x)
            val levelValues = allChildren.map(x=>x.value)
            nextLevel(allChildren, curList :+ levelValues)
        }
    }
    nextLevel(List(root), List())
  }
}