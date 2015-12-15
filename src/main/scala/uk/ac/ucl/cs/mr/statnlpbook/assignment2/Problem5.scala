package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable

/**
 * Created by Georgios on 30/10/2015.
 */

object Problem5{

  def main (args: Array[String]) {
    println("Joint Extraction")

    val train_dir = "./data/assignment2/bionlp/train"
    val test_dir = "./data/assignment2/bionlp/test"

    // load train and dev data
    // read the specification of the method to load more/less data for debugging speedup
    val (trainDocs, devDocs) = BioNLP.getTrainDevDocuments(train_dir,0.8,1000)
    // load test
    val testDocs = BioNLP.getTestDocuments(test_dir)
    // make tuples (Candidate,Gold)
    def preprocess(candidates: Seq[Candidate]) = candidates.map(e => e -> (e.gold,e.arguments.map(_.gold)))

    // ================= Joint Classification =================
    // get candidates and make tuples with gold
    // read the specifications of the method for different subsampling thresholds
    // no subsampling for dev/test!
    def getJointCandidates(docs: Seq[Document]) = docs.flatMap(_.jointCandidates(0.02,0.4))
    def getTestJointCandidates(docs: Seq[Document]) = docs.flatMap(_.jointCandidates())
    val jointTrain = preprocess(getJointCandidates(trainDocs))
    val jointDev = preprocess(getTestJointCandidates(devDocs))
    val jointTest = preprocess(getTestJointCandidates(testDocs))

    // show statistics for counts of true labels, useful for deciding on subsampling
    println("True label counts (trigger - train):")
    println(jointTrain.unzip._2.unzip._1.groupBy(x=>x).mapValues(_.length))
    println("True label counts (trigger - dev):")
    println(jointDev.unzip._2.unzip._1.groupBy(x=>x).mapValues(_.length))
    println("True label counts (argument - train):")
    println(jointTrain.unzip._2.unzip._2.flatten.groupBy(x=>x).mapValues(_.length))
    println("True label counts (argument - dev):")
    println(jointDev.unzip._2.unzip._2.flatten.groupBy(x=>x).mapValues(_.length))


    // get label sets
    val triggerLabels = jointTrain.map(_._2._1).toSet
    val argumentLabels = jointTrain.flatMap(_._2._2).toSet

    // define model
    //TODO: change the features function to explore different types of features
    //TODO: experiment with the unconstrained and constrained (you need to implement the inner search) models
    //val jointModel = JointUnconstrainedClassifier(triggerLabels,argumentLabels,Features.defaultTriggerFeatures,Features.defaultArgumentFeatures)
    val jointModel = JointConstrainedClassifier(triggerLabels,argumentLabels,Features.myTriggerFeatures,Features.myArgumentFeatures)

    // use training algorithm to get weights of model
    val jointWeights = PrecompiledTrainers.trainPerceptron(jointTrain,jointModel.feat,jointModel.predict,10)

    // get predictions on dev
    val jointDevPred = jointDev.unzip._1.map { case e => jointModel.predict(e,jointWeights) }
    val jointDevGold = jointDev.unzip._2

    // Triggers (dev)
    val triggerDevPred = jointDevPred.unzip._1
    val triggerDevGold = jointDevGold.unzip._1
    val triggerDevEval = Evaluation(triggerDevGold,triggerDevPred,Set("None"))
    println("Evaluation for trigger classification:")
    println(triggerDevEval.toString)

    // Arguments (dev)
    val argumentDevPred = jointDevPred.unzip._2.flatten
    val argumentDevGold = jointDevGold.unzip._2.flatten
    val argumentDevEval = Evaluation(argumentDevGold,argumentDevPred,Set("None"))
    println("Evaluation for argument classification:")
    println(argumentDevEval.toString)

    // get predictions on test
    val jointTestPred = jointTest.unzip._1.map { case e => jointModel.predict(e,jointWeights) }
    // Triggers (test)
    val triggerTestPred = jointTestPred.unzip._1
    // write to file
    Evaluation.toFile(triggerTestPred,"./data/assignment2/out/joint_trigger_test.txt")
    // Arguments (test)
    val argumentTestPred = jointTestPred.unzip._2.flatten
    // write to file
    Evaluation.toFile(argumentTestPred,"./data/assignment2/out/joint_argument_test.txt")
  }

}

/**
 * A joint event classifier (both triggers and arguments).
 * It predicts the structured event.
 * It's predict method should only produce the best solution that respects the constraints on the event structure.
 * @param triggerLabels
 * @param argumentLabels
 * @param triggerFeature
 * @param argumentFeature
 */
case class JointConstrainedClassifier(triggerLabels:Set[Label],
                                      argumentLabels:Set[Label],
                                      triggerFeature:(Candidate,Label)=>FeatureVector,
                                      argumentFeature:(Candidate,Label)=>FeatureVector
                                       ) extends JointModel {
  def predict(x: Candidate, weights: Weights) = {
    // get best label for given candidate and it's score
    def argmax(labels: Set[Label], x: Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
      val scores = labels.toSeq.map(y => y -> dot(feat(x, y), weights)).toMap withDefaultValue 0.0
      scores.maxBy(_._2)
    }
    // get all scores for trigger
    def triggerScores(labels: Set[Label], x: Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
      val scores = labels.toSeq.map(y => y -> dot(feat(x, y), weights)).toMap withDefaultValue 0.0
      //scores.toSeq.sortBy(- _._2) // best results first
      scores
    }
    // get argument with the best result for "Theme" (and it's result)
    def bestThemeArg(x:Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
      val scores = x.arguments.map(l => l -> dot(feat(l, "Theme"), weights)).toMap withDefaultValue 0.0
      scores.maxBy(_._2)
    }

    def reproduceArgLabels(argumentSeq: Seq[Candidate], labelsMap: Map[Candidate, Label], themeArg: Candidate) = {
      val argLabels = for (a <- argumentSeq ) yield {
        labelsMap.getOrElse(a, "Theme")
      }
      argLabels
    }

    // get sum of scores for all arguments labeled "None"
    val noneArgsScore = x.arguments.map(a => argmax(argumentLabels, a, weights, argumentFeature)._2).sum
    // get the best argument to be forced to be "Theme" and it's score
    val (themeArg, themeArgScore) = bestThemeArg(x, weights, argumentFeature)
    // exclude "Theme" argument from arguments
    val argsNoTheme = x.arguments.filter(a => !a.equals(themeArg))
    //exclude "Cause" from Argument Labels
    val argLabelsNoCause = argumentLabels.filter(l => !l.equals("Cause"))
    // get set of all regulation trigger Labels
    val regulationTriggerLabels = Set("Regulation", "Negative_regulation", "Positive_regulation")

    //find best labels for rest of arguments, sum score
    val bestArgLabels = argsNoTheme.map(a => a -> argmax(argumentLabels, a, weights, argumentFeature))
    val bestArgLabelsScore = bestArgLabels.map(a => a._2._2).sum

    // find best labels (excluding "Cause") for rest of arguments, sum score
    val bestArgLabelsNoCause = argsNoTheme.map(a => a -> argmax(argLabelsNoCause, a, weights, argumentFeature))
    val bestArgLabelsNoCauseScore = bestArgLabelsNoCause.map(a => a._2._2).sum

    // get scores for all trigger labels
    val triggerResults = triggerScores(triggerLabels, x, weights, triggerFeature)

    // find the best combinations and save them to map
    val scoresMap = new mutable.HashMap[(Label, Seq[Label]), Double]()

    // get results for None
    val noneArgLabels = for (a <- x.arguments) yield "None"
    scoresMap += ("None", noneArgLabels) -> (triggerResults("None") + noneArgsScore)

    // get results for event different than None
    val (bestNonNoneTriggerLabel, bestNonNoneTriggerScore) = triggerResults.filter(!_._1.equals("None")).maxBy(_._2)
    val nonNoneScore = bestNonNoneTriggerScore + themeArgScore + bestArgLabelsNoCauseScore
    scoresMap += (bestNonNoneTriggerLabel,
      reproduceArgLabels(argsNoTheme, bestArgLabelsNoCause.map(a => a._1 -> a._2._1).toMap, themeArg)) -> nonNoneScore

    // get results for regulation event
    val (bestRegulationTriggerLabel, bestRegulationTriggerScore) =
      triggerResults.filter(l => regulationTriggerLabels.contains(l._1)).maxBy(_._2)
    val regulationScore = bestRegulationTriggerScore + themeArgScore + bestArgLabelsScore
    scoresMap += (bestRegulationTriggerLabel,
      reproduceArgLabels(argsNoTheme, bestArgLabels.map(a => a._1 -> a._2._1).toMap, themeArg)) -> regulationScore

    scoresMap.toMap.maxBy(_._2)._1

  }


}

/**
 * A joint event classifier (both triggers and arguments).
 * It predicts the structured event.
 * It treats triggers and arguments independently, i.e. it ignores any solution constraints.
 * @param triggerLabels
 * @param argumentLabels
 * @param triggerFeature
 * @param argumentFeature
 */
case class JointUnconstrainedClassifier(triggerLabels:Set[Label],
                                        argumentLabels:Set[Label],
                                        triggerFeature:(Candidate,Label)=>FeatureVector,
                                        argumentFeature:(Candidate,Label)=>FeatureVector
                                         ) extends JointModel{
  /**
   * Constraint 1: if e=None, all a=None
   * Constraint 2: if e!=None, at least one a=Theme
   * Constraint 3: only e=Regulation can have a=Cause
   * @param x
   * @param weights
   * @return
   */
  def predict(x: Candidate, weights: Weights) = {
    def argmax(labels: Set[Label], x: Candidate, weights: Weights, feat:(Candidate,Label)=>FeatureVector) = {
      val scores = labels.toSeq.map(y => y -> dot(feat(x, y), weights)).toMap withDefaultValue 0.0
      scores.maxBy(_._2)._1
    }
    val bestTrigger = argmax(triggerLabels,x,weights,triggerFeature)
    val bestArguments = for (arg<-x.arguments) yield argmax(argumentLabels,arg,weights,argumentFeature)
    (bestTrigger,bestArguments)
  }

}

trait JointModel extends Model[Candidate,StructuredLabels]{
  def triggerFeature:(Candidate,Label)=>FeatureVector
  def argumentFeature:(Candidate,Label)=>FeatureVector
  def feat(x: Candidate, y: StructuredLabels): FeatureVector ={
    val f = new mutable.HashMap[FeatureKey, Double] withDefaultValue 0.0
    addInPlace(triggerFeature(x,y._1),f,1)
    for ((a,label)<- x.arguments zip y._2){
      addInPlace(argumentFeature(a,label),f,1)
    }
    f
  }
}


