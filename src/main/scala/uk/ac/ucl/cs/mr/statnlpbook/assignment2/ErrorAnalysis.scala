package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.util.Random
import scala.collection.mutable

/**
 * Created by Georgios on 16/11/2015.
 */
case class ErrorAnalysis(candidates:Seq[Candidate], golds:Seq[Label], preds:Seq[Label]) {
  val rng = new Random(101)
  val wrong = candidates.zip(golds.zip(preds)).filter(v=>v._2._1!=v._2._2).toIndexedSeq

  /**
    * Gives a brief report of mislabelling errors
    * @param num number of mislabelled examples to show
    */
  def showErrors(num:Int=1): Unit = for (_ <- 0 until num) {
    def prettyText(sentence: Sentence, begin:Int, end:Int, lineLength:Int = 1000)={
      val sb = new StringBuilder()
      for (i <- begin until end) {
        val word = sentence.tokens(i).word
        sb.append(word)
        if (i!=end-1) sb.append(" ")
        if ((i-begin) % lineLength == lineLength-1) sb.append("\n")
      }
      sb.toString
    }

    val idx = rng.nextInt(wrong.length)
    val candidate = wrong(idx)._1
    val gold = wrong(idx)._2._1
    val pred = wrong(idx)._2._2
    val isArgument = (candidate.parentIndex > -1)
    val candidateType = if (isArgument) "Argument" else "Trigger"
    val thisSentence = candidate.doc.sentences(candidate.sentenceIndex)
    val candidateText = prettyText(thisSentence,candidate.begin,candidate.end)
    val sentenceText = prettyText(thisSentence,0,thisSentence.tokens.length,20)
    println(s"${candidateType} candidate '${candidateText}' with true label '${gold}' mislabelled as '${pred}'")
    if (isArgument){
      val trigger = thisSentence.events(candidate.parentIndex)
      val triggerText =  prettyText(thisSentence,trigger.begin, trigger.end)
      println(s"Argument belongs to event with trigger '${triggerText}'")
    }
    println(s"Found in file '${candidate.doc.fileName}', in sentence:")
    println(s"${sentenceText}")
    println()
  }

  /**
    * Gives a brief error statistics report
    */
  def showErrorsStats(): Unit = {
    val counts = golds.groupBy(x => x).mapValues(_.length)
    val errors = new mutable.HashMap[(Label, Label), Double]().withDefaultValue(0.0)

    wrong.foreach(w => errors((w._2._1, w._2._2)) += 1)
    val errorSum = errors.map(e => e._2).foldLeft(0d)(_ + _)

    println()
    println("Classification error stats:")
    errors.toList.sortBy(-_._2).foreach(e => {
      println(f"${e._1._1} & ${e._1._2} & ${e._2} & ${e._2 / counts(e._1._1)}%1.2f & ${e._2 / errorSum}%1.2f\\\\")
      println("\\hline")
    })
  }


  /**
    * Gives a per-feature report of mislabelling errors
    * @param num number of mislabelled examples to show
    */
  def showErrors_p6(feat: (Candidate, StructuredLabels) => FeatureVector, weights: Weights, num:Int=1): Unit = for (_ <- 0 until num) {
    def prettyText(sentence: Sentence, begin:Int, end:Int, lineLength:Int = 1000)={
      val sb = new StringBuilder()
      for (i <- begin until end) {
        val word = sentence.tokens(i).word
        sb.append(word)
        if (i!=end-1) sb.append(" ")
        if ((i-begin) % lineLength == lineLength-1) sb.append("\n")
      }
      sb.toString
    }

    val idx = rng.nextInt(wrong.length)
    val candidate = wrong(idx)._1
    val gold = wrong(idx)._2._1
    val pred = wrong(idx)._2._2
    val isArgument = (candidate.parentIndex > -1)
    val candidateType = if (isArgument) "Argument" else "Trigger"
    val thisSentence = candidate.doc.sentences(candidate.sentenceIndex)
    val candidateText = prettyText(thisSentence,candidate.begin,candidate.end)
    val sentenceText = prettyText(thisSentence,0,thisSentence.tokens.length,20)
    //    println(s"${candidateType} candidate '${candidateText}' with true label '${gold}' mislabelled as '${pred}'")
    println()
    println(s"- ${candidateType} candidate '${candidateText}':")
//    if (isArgument){
//      val trigger = thisSentence.events(candidate.parentIndex)
//      val triggerText =  prettyText(thisSentence,trigger.begin, trigger.end)
//      println(s"Argument belongs to event with trigger '${triggerText}'")
//    }
    //    println(s"Found in file '${candidate.doc.fileName}', in sentence:")
    //    println(s"${sentenceText}")
    val f = (x: Candidate, l: Label) => feat(x, new StructuredLabels(l, List()))

    println()
    println("[" + pred + "] " + "%1.3f".format(dot(f(candidate, pred), weights)) + " (predicted)")
    f(candidate, pred)
      .map(f => "[" + f._1.template + "]" + f._1.arguments.take(f._1.arguments.length - 1).foldLeft("")(_ + " - " + _) -> f._2 * weights(f._1))
      .toList
      .sortBy(p => -math.abs(p._2))
      .foreach(p => println("%1.3f".format(p._2) + ": " + p._1))

    println()
    println("[" + gold + "] " + "%1.3f".format(dot(f(candidate, gold), weights)) + " (gold)")
    f(candidate, gold)
      .map(f => "[" + f._1.template + "]" + f._1.arguments.take(f._1.arguments.length - 1).foldLeft("")(_ + " - " + _) -> f._2 * weights(f._1))
      .toList
      .sortBy(p => -math.abs(p._2))
      .foreach(p => println("%1.3f".format(p._2) + ": " + p._1))

    println()
    println(s">> ${sentenceText}")
    println()
  }
}
