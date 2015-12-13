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
        val mentionsFiltered = mentions.filter(m => (m.begin <= mod && m.end > mod))
        if (mentionsFiltered.size > 0) {
          protein = mentionsFiltered.head.label
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
    feats += FeatureKey("first trigger word length", List(token.word.size.toString, y)) -> 1.0
    feats += FeatureKey("first trigger word pos", List(token.pos, y)) -> 1.0

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
      feats += FeatureKey("distance to mention right",
        List(mentionsAfter.head.label, (mentionsAfter.head.begin - end).toString, y)) -> 1.0

    }
    if (!mentionsBefore.isEmpty) {
      feats += FeatureKey("distance to mention left",
        List(mentionsBefore.last.label, (begin - mentionsBefore.last.begin).toString, y)) -> 1.0
    }

    feats += FeatureKey("number of mention in sentence", List(thisSentence.mentions.size.toString + y)) -> 1.0

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

    val feats = new mutable.HashMap[FeatureKey,Double]
    feats += FeatureKey("label bias", List(y)) -> 1.0
    val token = thisSentence.tokens(begin) //first word of argument

    feats += FeatureKey("first source word", List(eventHeadToken.word, y)) -> 1.0
    feats += FeatureKey("first source pos", List(eventHeadToken.pos, y)) -> 1.0
    feats += FeatureKey("is destination a protein", List(x.isProtein.toString, y)) -> 1.0

    if (!x.isProtein) {
      feats += FeatureKey("first destination word pos", List(token.pos, y)) -> 1.0
      feats += FeatureKey("first destination word", List(token.word, y)) -> 1.0
    }

    val depsFrom = thisSentence.deps.filter(d => {
      (d.head >= begin && d.head < end && d.mod >= event.begin && d.mod < event.end)
    }).map(d => d.label)
    depsFrom.foreach(d => {
      feats += FeatureKey("dependency from destination to source", List(d, y)) -> 1.0
    })
    feats += FeatureKey("dependency d-s aggregated", List(depsFrom.size.toString, y)) -> 1.0

    val depsTo = thisSentence.deps.filter(d => {
      (d.head >= event.begin && d.head <= event.end && d.mod >= begin && d.mod < end)
    }).map(d => d.label)
    depsTo.foreach(d => {
      feats += FeatureKey("dependency from source to destination", List(d, y)) -> 1.0
    })
    feats += FeatureKey("dependency s-d aggregated", List(depsTo.size.toString, y)) -> 1.0

    //distance from the source to destination
    val distance = begin - event.begin
    feats += FeatureKey("distance between source and destination", List(distance.toString, y)) -> 1.0

    //bigram near destination
    if (begin > 0) {
      val before = thisSentence.tokens(begin - 1)
      feats += FeatureKey("bigram before destination", List(before.word, thisSentence.tokens(begin).word, y)) -> 1.0
      feats += FeatureKey("unigram before destination", List(before.word, y)) -> 1.0
    }
    if (end  < thisSentence.tokens.size) {
      val next = thisSentence.tokens(end)
      feats += FeatureKey("bigram after destination", List(next.word, thisSentence.tokens(end-1).word, y)) -> 1.0
      feats += FeatureKey("unigram after destination", List(next.word, y)) -> 1.0
    }
    // bigram near source
    if (event.begin > 0) {
      val before = thisSentence.tokens(event.begin - 1)
      feats += FeatureKey("bigram before source", List(before.word, eventHeadToken.word, y)) -> 1.0
      feats += FeatureKey("unigram before source", List(before.word, y)) -> 1.0
    }
    if (event.end < thisSentence.tokens.size) {
      val next = thisSentence.tokens(event.end)
      feats += FeatureKey("bigram after source", List(next.word, thisSentence.tokens(event.end - 1).word, y)) -> 1.0
      feats += FeatureKey("unigram after source", List(next.word, y)) -> 1.0
    }

    //try to consider number of proteins
    val mentions = thisSentence.mentions.filter(m => {
      val start = Math.min(begin, event.begin)
      val stop = Math.max(end, event.end)
      (m.begin >= start && m.begin < stop) && (m.end >= event.begin && m.end < stop)
    }).map(m => m.label)
    feats += FeatureKey("number of mentions between source and destination", List(mentions.size + y)) -> 1.0

//    val dependencyGraph = buildDependencyGraph(thisSentence.tokens.size, thisSentence.deps)
//    val pathDestToProtein = shortestPathToProtein(begin, end, dependencyGraph, thisSentence.mentions)
//    feats += FeatureKey("shortest path from destination to protein",
//      List(pathDestToProtein._1, pathDestToProtein._2.toString, y)) -> 1.0
//
//    val pathSourceToProtein = shortestPathToProtein(event.begin, event.end, dependencyGraph, thisSentence.mentions)
//    feats += FeatureKey("shortest path from source to protein",
//      List(pathSourceToProtein._1, pathSourceToProtein._2.toString, y)) -> 1.0

    feats.toMap
  }

}
