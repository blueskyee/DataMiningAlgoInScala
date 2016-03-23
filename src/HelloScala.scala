/**
  * Created by blueskyee on 2016/3/19.
  */
object HelloScala {

  def main(args: Array[String]): Unit = {
    val data = Array(
    "21,1,0,0",
    "22,2,1,0",
    "21,3,2,1"
    )

    val sumfunc = (x:Int, y:Int) => x + y

    val results = data.map{str =>
    val p = str.split(",")
    (p(0).toInt, p.drop(1).map(_.toInt).sum)
    }.groupBy(_._1).map{keyvalue =>
      (keyvalue._1, keyvalue._2.map(_._2).reduceLeft(sumfunc))
    }

    for (r <- results) println(r._1 + "," + r._2)
  }

}
