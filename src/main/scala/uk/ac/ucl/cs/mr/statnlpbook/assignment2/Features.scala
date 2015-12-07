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

  def dependencyToProteinRight (dep : List[Dependency], men : IndexedSeq[Mention]): String = {
    var result = ""
    var minLength = 400;
    var index = 0
    if (dep.size > 0) {
      for (i <- 0 to dep.size-1) {
        // check if there is any dependency to or from the right side of token
        if (dep(i).head < dep(i).mod) {
          // find the shortes path
          if ( (dep(i).mod - dep(i).head) < minLength) {
            for (k <- 0 to men.size - 1) {
              if ( (dep(i).head >= men(k).begin && dep(i).head < men(k).end) || (dep(i).mod >= men(k).begin && dep(i).mod < men(k).end) ) {
                minLength = dep(i).mod - dep(i).head
                index = i
                result = dep(index).label
              }
            }
          }
        }
      }
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
    feats += FeatureKey("first trigger word length", List(token.word, y)) -> token.word.size //word size
    feats += FeatureKey("first trigger word pos", List(token.pos, y)) -> 1.0

    feats += FeatureKey("is upper case", List(containUpperCase(token.word).toString, y)) -> 1.0
    feats += FeatureKey("is digit", List(containDigit(token.word).toString, y)) -> 1.0


    if (begin > 0) {
      val before = thisSentence.tokens(begin-1)
      feats += FeatureKey("bigram before", List(before.word, token.word, y)) -> 1.0
    }
    if (begin + 1 < thisSentence.tokens.size) {
      val next = thisSentence.tokens(begin+1)
      feats += FeatureKey("bigram after", List(next.word, token.word, y)) -> 1.0
    }

    val mentionsSorted = thisSentence.mentions.sortBy(m => m.begin)
    val mentionsAfter = mentionsSorted.filter(m => m.begin >= end)
    val mentionsBefore = mentionsSorted.filter(m => m.begin <= begin)

    if (!mentionsAfter.isEmpty) {
      feats += FeatureKey("distance to mention right", List(y)) -> (mentionsAfter.head.begin - end)
    }
    if (!mentionsBefore.isEmpty) {
      feats += FeatureKey("distance to mention left", List(y)) -> (begin - mentionsBefore.last.begin)
    }
    val mentionsSent = thisSentence.mentions.map(m => m.label)
    feats += FeatureKey("number of mention in sentence", List(mentionsSent + y)) -> mentionsSent.size

    val dep = thisSentence.deps.filter(d => {
      d.mod == begin || d.head == begin
    })
    feats += FeatureKey("bdsdsadad", List(dependencyToProteinRight(dep, thisSentence.mentions), y)) -> 1.0

    val mentions = thisSentence.mentions.filter(m => {
      (m.begin >= begin && m.begin <= end) || (m.end >= m.begin &&  m.end <= end)
    }).map(m => m.label)
    feats += FeatureKey("number of mention in frame", List(mentions + y)) -> mentions.size

    val depsHeads = thisSentence.deps.filter(d => {
      d.head == begin
    }).map(d => d.label)
    depsHeads.foreach(d => {
      feats += FeatureKey("dependency head", List(d, y)) -> 1.0
    })
    //feats += FeatureKey("number of dependency heads", List(depsHeads + y)) -> depsHeads.size

    val depsMods = thisSentence.deps.filter(d => {
      d.mod == begin
    }).map(d => d.label)
    depsMods.foreach(d => {
      feats += FeatureKey("dependency mod", List(d, y)) -> 1.0
    })
    //feats += FeatureKey("number of dependency mods", List(depsMods + y)) -> depsMods.size

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
