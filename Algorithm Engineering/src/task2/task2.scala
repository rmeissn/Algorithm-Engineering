package task2

import scala.collection.parallel.ParSeq

object task2 {

  def main(args: Array[String]): Unit = {
    val gsize = 5E4
    val wordLength = 40
    val mkset = Array("a", "b", "c")

    for (i <- 1 to 6) {
      val size = gsize * i
      val res = calcResuslts(measure(30, wordLength, size, mkset, quickSortNewTuned))
      println("\nLänge der Liste: " ++ size.toInt.toString)
      println("Maximal: " ++ res._1)
      println("Minimal: " ++ res._2)
      println("Durchschnitt: " ++ res._3)
      println("Abweichung: " ++ res._4)
    }
  }

  def measure[A](count: Int, l: Int, n: Double, mkset: Array[String], f: IndexedSeq[String] => IndexedSeq[String]): ParSeq[Long] = {
    var measurements = ParSeq[Long]()
    val help = ParSeq.range(0, n.toInt)

    def fill(l: Int, times: Int): String = {
      mkset(math.round((math.random * (mkset.length - 1)).toFloat)) ++
        (if (times < l) fill(l, times + 1) else "")
    }
    System.gc()
    val randomSeq = (help map (_ => fill(((math.random * 0.5 + 0.5) * l) toInt, 0))).toVector
    println("Round: ")

    for (s <- 1 to count) {
      print(s + " ")
      val start = System.currentTimeMillis()
      val sortedSeq = f(randomSeq)
      val stop = System.currentTimeMillis()
      if (sortedSeq.foldLeft("a") { (x, y) => if (x <= y) y else "z" } != sortedSeq.last ||
        sortedSeq.length != n.toInt) {
        println("Failure")
        System.exit(1)
      }
      measurements :+= (stop - start)
      System.gc()
    }
    measurements
  }

  def calcResuslts(seq: ParSeq[Long]): (String, String, String, String) = {
    val smallseq = seq.drop(10)
    val middle = (smallseq.sum / smallseq.length)
    return ((smallseq.max).toString, (smallseq.min).toString, middle.toString,
      Math.sqrt(((smallseq.map(x => Math.pow(x - middle, 2))).sum / smallseq.length)).toString)
  }

  def quickSortOld(seq: IndexedSeq[String]): IndexedSeq[String] = seq match {
    case head :+ tail =>
      val (left, right) = head partition (tail >)
      quickSortOld(left) ++ (tail +: quickSortOld(right))
    case list => list
  }

  def quickSortNew(seq: IndexedSeq[String]): IndexedSeq[String] = seq match {
    case head +: seq :+ tail =>
      var pivot1 = head; var pivot2 = tail;
      if (head > tail) { val tmp = pivot2; pivot2 = pivot1; pivot1 = tmp; }
      val (left, tmp) = seq partition (pivot1 >)
      val (right, middle) = tmp partition (pivot2 <)
      quickSortNew(left) ++ (pivot1 +: quickSortNew(middle)) ++ (pivot2 +: quickSortNew(right))
    case list => list
  }
  
  def quickSortNewTuned(seq: IndexedSeq[String]): IndexedSeq[String] = {
    if(seq.length < 2) seq
    else if (seq.length < 100) insertionSort(seq)
    else{
      var pivot1 = seq.head; var pivot2 = seq.last;
      if (pivot1 > pivot2) { val tmp = pivot2; pivot2 = pivot1; pivot1 = tmp; }
      val (left, tmp) = seq.init.tail partition (pivot1 >)
      val (right, middle) = tmp partition (pivot2 <)
      quickSortNewTuned(left) ++ (pivot1 +: quickSortNewTuned(middle)) ++ (pivot2 +: quickSortNewTuned(right))
    }
    
  }

  def insertionSort(seq: IndexedSeq[String]) = {
    def insert(list: IndexedSeq[String], value: String) = list span (value >) match {
      case (lower, upper) => lower ++ (value +: upper)
    }
    seq.foldLeft(IndexedSeq[String]())(insert)
  }

  def extractPivot(seq: IndexedSeq[String]): (IndexedSeq[String], String) = {
    def findPivot(seq: IndexedSeq[String]): Int = {
      val p = Array(seq.head, seq((seq.length / 2).toInt), seq.last)
      val x = (p.sorted); if (x(1) == seq.head) 0 else if (x(1) == seq.last) seq.length - 1 else (seq.length / 2).toInt
    }
    val id = findPivot(seq)
    val (left, pivot +: right) = seq.splitAt(id)
    return (left ++ right, pivot)
  }
}