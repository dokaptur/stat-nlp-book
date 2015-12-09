package uk.ac.ucl.cs.mr.statnlpbook.assignment2


import cc.factorie.app.nlp.coref.CorefFeatures.False

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

  def dependencyToProteinRight (dep : List[Dependency], men : IndexedSeq[Mention]): (String, Double) = {
    var result = ""
    var minLength = 400;
    var index = 0
    if (dep.size > 0) {
      for (i <- 0 to dep.size-1) {
        if ( (dep(i).mod - dep(i).head) < minLength) {
          for (k <- 0 to men.size - 1) {
            if ( (dep(i).head >= men(k).begin && dep(i).head < men(k).end) ||
              (dep(i).mod >= men(k).begin && dep(i).mod < men(k).end) ) {
              minLength = dep(i).mod - dep(i).head
              index = i
              result = dep(index).label
            }
          }
        }
      }
    }
    if (result.isEmpty) {
      minLength = 0
    }
    result -> minLength
  }

  def shortestPathToProtein (begin: Int, end: Int, depsGraph: Map[Int, List[Dependency]],
                             mentions: IndexedSeq[Mention]): (String, Double) = {
    var protein = ""
    var distance = 0.0
    val queue = mutable.Queue[(Dependency, Double)]()
    val visited = mutable.HashMap[Int, Boolean]().withDefaultValue(false)
    for (i <- begin until end) {
      queue.enqueue(depsGraph.getOrElse(i, List[Dependency]()).map(f => (f -> 1.0)):_*)
      visited(i) = true
    }
    var found = false
    while (!found && queue.size > 0) {
      val (dep, dist) = queue.dequeue()
      val mod = dep.mod
      if (!visited(mod)) {
        visited(mod) = true
        // check if mod is a protein
        if (mentions.map(m => m.begin).contains(mod)) {
          protein = mentions.filter(m => m.begin == mod).head.label
          distance = dist
          found = true
        }
        queue.enqueue(depsGraph.getOrElse(mod, List[Dependency]()).map(f => (f -> (dist + 1.0))):_*)
      }
    }
    protein -> distance
  }

  def buildDependencyGraph(size: Int, deps: List[Dependency]) : Map[Int, List[Dependency]] = {
    val hashMap = new mutable.HashMap[Int, mutable.ListBuffer[Dependency]]()
      .withDefaultValue(mutable.ListBuffer[Dependency]())
    for (i <- 0 until size-1) {
      deps.foreach(d => {
        if (d.head == i) {
          hashMap(i) += d
        }
      })
    }
    hashMap.map(f => (f._1 -> f._2.toList)).toMap
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
    feats += FeatureKey("first trigger word length", List(token.word, y)) -> token.word.size
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
      feats += FeatureKey("distance to mention right", List(mentionsAfter.head.label, y)) ->
        (mentionsAfter.head.begin - end)
    }
    if (!mentionsBefore.isEmpty) {
      feats += FeatureKey("distance to mention left", List(mentionsBefore.last.label, y)) ->
        (begin - mentionsBefore.last.begin)
    }

    feats += FeatureKey("number of mention in sentence", List(thisSentence.mentions.map(m => m.label) + y)) ->
      thisSentence.mentions.size

    val mentions = thisSentence.mentions.filter(m => {
      (m.begin >= begin && m.begin <= end) || (m.end >= m.begin &&  m.end <= end)
    }).map(m => m.label)
    feats += FeatureKey("number of mention in frame", List(mentions + y)) -> mentions.size

    val dependencyGraph = buildDependencyGraph(thisSentence.tokens.size, thisSentence.deps)
    val pathToProtein = shortestPathToProtein(begin, end, dependencyGraph, thisSentence.mentions)
    feats += FeatureKey("shortest path to protein", List(pathToProtein._1, pathToProtein._2.toString, y)) -> 1.0

    val dep = thisSentence.deps.filter(d => {
      (d.mod == begin || d.head == begin) && (d.head < d.mod)
    })
    val depToProteinRight = dependencyToProteinRight(dep, thisSentence.mentions)
    feats += FeatureKey("dependency to protein right",
      List(depToProteinRight._1, depToProteinRight._2.toString, y)) -> 1.0


    val depHeads = thisSentence.deps.filter(d => {
      d.head == begin
    })
    depHeads.foreach(d => {
      feats += FeatureKey("dependency head", List(d.label, y)) -> 1.0
    })

    val depMods = thisSentence.deps.filter(d => {
      d.mod == begin
    })
    depMods.foreach(d => {
      feats += FeatureKey("dependency mod", List(d.label, y)) -> 1.0
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

    val eventEnd = (begin + end) / 2
    val eventEndToken = thisSentence.tokens(eventEnd) // the "destination" of the argument

    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument
    feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
    feats += FeatureKey("is protein_first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0
    //feats += FeatureKey("first argument word", List(token.word, y)) -> 1.0
   // feats += FeatureKey("first trigger word pos", List(token.pos, y)) -> 1.0
    //feats += FeatureKey("first trigger word stem", List(token.stem, y)) -> 1.0

    feats += FeatureKey("end word", List(eventEndToken.word, y)) -> 1.0
    feats += FeatureKey("head word", List(eventHeadToken.word, y)) -> 1.0
    feats += FeatureKey("end pos ", List(eventEndToken.pos, y)) -> 1.0
    feats += FeatureKey("head pos", List(eventHeadToken.pos, y)) -> 1.0

   // feats += FeatureKey("second argument word", List(eventHeadToken.word, y)) -> 1.0
   // feats += FeatureKey("second trigger word pos", List(eventHeadToken.pos, y)) -> 1.0
    //feats += FeatureKey("second trigger word stem", List(eventHeadToken.stem, y)) -> 1.0

   // feats += FeatureKey("is protein first trigger word", List(x.isProtein.toString,eventHeadToken.word, y)) -> 1.0
   // feats += FeatureKey("is protein second trigger word", List(x.isProtein.toString,token.word, y)) -> 1.0

    val depsFrom = thisSentence.deps.filter(d => {
      //(d.head >= begin && d.head < end && d.mod >= event.begin && d.mod < event.end)
      d.head ==  event.begin && d.mod == eventEnd
    }).map(d => d.label)
    depsFrom.foreach(d => {
      feats += FeatureKey("dependency from trigger to parent", List(d, y)) -> 1.0
    })
    feats += FeatureKey("dependency t-p aggregated", List(depsFrom + y)) -> depsFrom.size
    val depsTo = thisSentence.deps.filter(d => {
      //(d.head >= event.begin && d.head <= event.end && d.mod >= begin && d.mod < end)
      d.head == eventEnd && d.mod == event.begin
    }).map(d => d.label)
    depsTo.foreach(d => {
      feats += FeatureKey("dependency from parent to trigger", List(d, y)) -> 1.0
    })
    feats += FeatureKey("dependency p-t aggregated", List(depsTo + y)) -> depsTo.size

    //distance from the candidate to the potential argument
    //val distance = begin - event.begin
   // feats += FeatureKey("distances between trigger and arguments", List(eventHeadToken.word, token.word, y)) -> distance


    val distance = (begin + end) / 2 - event.begin
    feats += FeatureKey("distances between trigger and arguments", List(distance.toString, y)) -> 1


    //bigram near the candidate
    if (begin > 0) {
      val before = thisSentence.tokens(eventEnd - 1)
      feats += FeatureKey("bigram before event trigger", List(before.word, token.word, y)) -> 1.0
    }
    if (begin + 1 < thisSentence.tokens.size) {
      val next = thisSentence.tokens(eventEnd + 1)
      feats += FeatureKey("bigram after event trigger", List(next.word, token.word, y)) -> 1.0
    }
    // bigram near the potential argument
    if (event.begin > 0) {
      val before = thisSentence.tokens(event.begin - 1)
      feats += FeatureKey("bigram before potential argument", List(before.word, eventHeadToken.word, y)) -> 1.0
    }
    if (event.begin + 1 < thisSentence.tokens.size) {
      val next = thisSentence.tokens(event.begin + 1)
      feats += FeatureKey("bigram after potential argument", List(next.word, eventHeadToken.word, y)) -> 1.0
    }

    //try to consider number of proteins
    val mentions = thisSentence.mentions.filter(m => {
      //(m.begin >= event.end && m.begin <= begin) || (m.end >= event.begin && m.end <= end)
      true
    }).map(m => m.label)
    feats += FeatureKey("number of mentions in sentence", List(mentions + y)) -> mentions.size

    val dependencyGraph = buildDependencyGraph(thisSentence.tokens.size, thisSentence.deps)
    val pathToProtein = shortestPathToProtein(begin, end, dependencyGraph, thisSentence.mentions)
    feats += FeatureKey("shortest path to protein", List(pathToProtein._1, y)) -> pathToProtein._2

    feats.toMap
  }


}
