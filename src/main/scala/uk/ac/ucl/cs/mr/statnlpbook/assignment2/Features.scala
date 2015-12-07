package uk.ac.ucl.cs.mr.statnlpbook.assignment2

import scala.collection.mutable

/**
 * Created by Georgios on 05/11/2015.
 */

object Features {

  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Trigger Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature
    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats.toMap
  }
  /**
   * a feature function with two templates w:word,label and l:label.
   * Example for Argument Exraction
   * @param x
   * @param y
   * @return
   */
  def defaultArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0
    feats.toMap
  }

  def containUpperCase(word : String) : Int = {
    var result = 0
    for (i <- 0 to word.length - 1) {
      if (word(i).isUpper)
        result = 1
    }
    result
  }


  def containDigit(word : String) : Int = {
    var result = 0
    for (i <- 0 to word.length - 1) {
      if (word(i).isDigit)
        result = 1
    }
    result
  }
  
  //TODO: make your own feature functions
  def myTriggerFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex) //use this to gain access to the parent sentence
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0 //bias feature

    val token = thisSentence.tokens(begin) //first token of Trigger
    feats += FeatureKey("first trigger word", List(token.word, y)) -> 1.0 //word feature
    feats += FeatureKey("first trigger word stem", List(token.stem, y)) -> 1.0 //word stem feature
    feats += FeatureKey("first trigger word pos", List(token.pos, y)) -> 1.0


    //feats += FeatureKey("is upper case", List(containUpperCase(token.word).toString, y)) -> 1.0
    //feats += FeatureKey("is digit", List(containDigit(token.word).toString, y)) -> 1.0


    if (begin > 0) {
      val before = thisSentence.tokens(begin-1)
      feats += FeatureKey("bigram before", List(before.word, token.word, y)) -> 1.0
    }
    if (begin + 1 < thisSentence.tokens.size) {
      val next = thisSentence.tokens(begin+1)
      feats += FeatureKey("bigram after", List(next.word, token.word, y)) -> 1.0
    }



    val mentions = thisSentence.mentions.filter(m => {
      (m.begin >= begin && m.begin <= end) || (m.end >= m.begin &&  m.end <= end)
    }).map(m => m.label)
    feats += FeatureKey("number of mention", List(mentions + y)) -> mentions.size

    thisSentence.deps.filter(d => {
      d.head == begin
    }).map(d => d.label).foreach(d => {
      feats += FeatureKey("dependency head", List(d, y)) -> 1.0
    })

    thisSentence.deps.filter(d => {
      d.mod == begin
    }).map(d => d.label).foreach(d => {
      feats += FeatureKey("dependency mod", List(d, y)) -> 1.0
    })

    feats.toMap
  }

  def myArgumentFeatures(x: Candidate, y: Label): FeatureVector = {
    val doc = x.doc
    val begin = x.begin
    val end = x.end
    val thisSentence = doc.sentences(x.sentenceIndex)
    val event = thisSentence.events(x.parentIndex) //use this to gain access to the parent event
    val eventHeadToken = thisSentence.tokens(event.begin) //first token of event
    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0

    val deps = thisSentence.deps.filter(d => {
      (d.head >= begin && d.head < end) || (d.mod >= begin && d.mod < end)
    }).map(d => d.label)

    deps.foreach(d => {
      feats += FeatureKey("dependancy modifier", List(d, y)) -> 1.0
    })


    feats.toMap
  }


}
