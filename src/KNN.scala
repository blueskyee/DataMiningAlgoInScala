/**
  * Algorithm: K Nearest Neighbor
  * Input:
  *   - training data: Array[(GroupID, Features)] => Array[(Int, Array[Double])]
  *   - predict data: Array[Features] => Array[Array[Double]]
  *   - reference number: K:Int
  * Output:
  *   predicted GroupID => Array[Int]
  *
  * Created by bluesky on 2016/3/20.
  */
object KNN {

  def main(args: Array[String]): Unit = {
    val predict_result = KNNCore(createTrainData(),createPredictData(),3)

    var predict_idx = 1
    for(x<-predict_result){
      println("record " + predict_idx + " predicted group id:" + x)
      predict_idx+=1
    }
  }

  //create train data
  def createTrainData(): Array[(Int, Array[Double])] = {
    val train_data = Array(
      (0,Array(7.1,0.0,0.0,0.0,0.0)),
      (0,Array(0.0,8.2,0.0,0.0,0.0)),
      (0,Array(0.0,0.0,9.3,0.0,0.0)),
      (1,Array(0.0,0.0,0.0,10.4,0.0)),
      (1,Array(0.0,0.0,0.0,0.0,11.5))
    )
    return train_data
  }

  //create predict data
  def createPredictData(): Array[Array[Double]] = {
    val predict_data = Array(
      Array(10.0,12.5,0.0,0.0,0.0),
      Array(0.0,0.0,0.0,2.1,5.6)
    )
    return predict_data
  }

  // --- Start KNN Function ---
  def KNNCore(
               // - Input Data
               //   TrainData : Array[(GroupID, Features)]
               TrainData : Array[(Int, Array[Double])],
               //   PredictData : Array[Features]
               PredictData : Array[Array[Double]],
               //   K : Reference Number
               K: Int
               // - Output Data : Array[GroupID]
             ): Array[Int] = {

    // - Write Your Code Here
    // ----------------------
    var result_ary = new Array[Int](PredictData.length)

    //find knn by calculating cosine similarity between predict data and training data
    for(predidx <- 0 to (PredictData.length-1)){
      val knn_cos_value = similarityFromTrainData(TrainData ,PredictData(predidx))
                          .toSeq.sortBy(-_._2).take(K)

      val knn_grp_id = knn_cos_value.groupBy(_._1).mapValues(_.size)

      result_ary(predidx) = knn_grp_id.toSeq.sortBy(-_._2).map(x=>x._1).head
    }

    return result_ary
  }

  def similarityFromTrainData(
                  train_data:Array[(Int, Array[Double])],
                  predict_data:Array[Double]
                ): Array[(Int,Double)] = {
    train_data map (keyvale=>(keyvale._1,calcCosineSimilarity(keyvale._2,predict_data)))
  }

  //calculate cosine similarity
  def calcCosineSimilarity(
                            v1:Array[Double],
                            v2:Array[Double]
                          ): Double = {
    require(v1.size == v2.size)
    dotProduct(v1, v2)/(magnitude(v1) * magnitude(v2))
  }

  def dotProduct(x: Array[Double], y: Array[Double]): Double = {
    (for((a, b) <- x zip y) yield a * b) sum
  }

  def magnitude(x: Array[Double]): Double = {
    math.sqrt(x map(i => i*i) sum)
  }

}
