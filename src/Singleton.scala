import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object Singleton {

  case class Team(name: String, region: String, rank: Int)
  
  case class Group(teams: List[Team]){
    def canBeAdded(team: Team): Boolean = {
      teams.filter(_.region==team.region).size==0 && teams.size<3
    }
    def addToGroup(team: Team, lGroup: Group): Group = Group(lGroup.teams :+ team)
  }
  
  case class GroupConfiguration(groups: List[Group]){
    def canBeAdded(team: Team): Boolean = {
      groups.filter(_.canBeAdded(team)).size>0
    }
    def addToGroupConfig(team: Team, failed: List[Group], lGroupConf: GroupConfiguration): Option[GroupConfiguration] = lGroupConf.groups match{
      case Nil => None
      case x :: xs => if(x.canBeAdded(team)){
        Some(GroupConfiguration(failed ++ xs :+ x.addToGroup(team, x)))
      } else {
        addToGroupConfig(team, failed :+ x, GroupConfiguration(xs))
      }
    }
    
    def printGC(): Unit = {
      groups.foreach(x=> println(x.teams.map(y=>y.name + " ").reduce((x,y)=> x + y)))
      println()
    }
  }
  
  val gList = ListBuffer[GroupConfiguration]()
  val gFList = ListBuffer[GroupConfiguration]()

  val eu = 10
  val na = 10
  val cn = 12
  val tw = 9
  val sea = 6
  val kr = 16
  val first = 11
  val second = 10
  val third = 9
  
  
  case class TeamNames(name: String, teams: List[String], rank: Int)
  
  def main(args: Array[String]): Unit = {
    val seed1s = Team("G2", "EU", eu*first) :: Team("LZ", "KR", kr*first) :: Team("FW", "TW", tw*first) :: Team("EDG", "CN", cn*first) :: Nil
    val seed2s = Team("MSF", "EU", eu*second) :: Team("SKT", "KR", kr*second) :: Team("SSG", "KR", kr*third) :: Team("AHQ", "TW", tw*second) :: 
      Team("RNG", "CN", cn*second) :: Team("GBM", "SEA", sea*first) :: Team("TSM", "NA", na*first) :: Team("IMT", "NA", na*second) :: Nil
    val groups = Group(List()) :: Group(List()) :: Group(List()) :: Group(List()) :: Nil
    genPossPools(seed1s, GroupConfiguration(groups), gList)
    gList.foreach(genPossPools(seed2s, _, gFList))
    genPossPools(seed2s, gList.head, gFList)
    
    
    
    val allGroups = gFList.toList
    
//    println(allGroups.filter(x=>x.groups.filter(y=>y.teams.size==3).size>0).size)
    
//    println(gFList.take(20).map(x=>x.printGC))
    val allTeamAvgs = allGroups.flatMap(x=>x.groups.flatMap{y=>
      val groupDiff = y.teams.foldLeft(0)((a, b) => a+b.rank)
      val allTeams = y.teams.foldLeft(List[String]())((a, b) => a.+:(b.name))
      y.teams.map(a=>TeamNames(a.name, allTeams, groupDiff-a.rank))
    })
    val maxMinTotalCount = allTeamAvgs.groupBy(x=>x.name).map(x=>(x._1, x._2
        .foldLeft((TeamNames("", List(), Int.MinValue), TeamNames("", List(), Int.MaxValue), 0, 0))
        ((a, b) => if(b.rank>a._1.rank && b.rank<a._2.rank) (b, b, a._3+b.rank, a._4 + 1) else if(b.rank<=a._1.rank && b.rank<a._2.rank) (a._1, b, a._3+b.rank, a._4+1)
        else if(b.rank>a._1.rank) (b, a._2, a._3+b.rank, a._4+1) else (a._1, a._2, a._3+b.rank, a._4+1))))
//    val allTeamAvgs = allGroups.flatMap(x=>x.groups.find(p=>(p.teams.find(z=>z.name=="TSM").isDefined))).map(x=>x.teams.map(y=>y.rank).sum/3)
//    val overallAvg = allTeamAvgs.sum/allTeamAvgs.count
//    println(overallAvg)
    val orderedMaxMinAvg = maxMinTotalCount.map(x=>(x._1, x._2._1.teams.reduce((a,b)=>a+" " + b) + " " + x._2._1.rank, x._2._2.teams.reduce((a,b)=>a+" " + b) + " " + x._2._2.rank, (x._2._3/x._2._4))).toList.sortBy(f=>f._4)
    orderedMaxMinAvg.map(x=>x._1 + "|" + x._2 + "|" + x._3 + "|" +  x._4).foreach(println)
//    maxMinTotalCount.foreach(x=>println(x._1, x._2._1, x._2._2, x._2._3/x._2._4))
    
    val stat = allGroups.filter(x=>x.groups.find(p=>(p.teams.find(z=>z.name=="G2").isDefined && !p.teams.find(z=>z.region=="KR").isDefined)).isDefined).size
    
    println(stat, allGroups.size, stat.toDouble/allGroups.size)
    
  }
  
  def genPossPools(seed1s: List[Team], last: GroupConfiguration, l : ListBuffer[GroupConfiguration]): Unit = seed1s match {
    case Nil => {
      l += last
    }
    case _ => seed1s.zipWithIndex.foreach{case (x, index)=> if(last.canBeAdded(x)){
//        println(seed1s)
        val gc = last.addToGroupConfig(x, List(), last)
        if(gc.isDefined){
          genPossPools(seed1s.take(index)++seed1s.drop(index+1), gc.get, l)
        }
      }
    }
  }
}