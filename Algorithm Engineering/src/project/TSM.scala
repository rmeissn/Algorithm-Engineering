package project

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

import scala.Array.canBuildFrom
import scala.collection.immutable.HashMap
import scala.collection.immutable.Vector
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.immutable.ParSet
import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel.traversable2ops
import scala.math.Pi
import scala.math.acos
import scala.math.cos
import scala.math.sin
import scala.sys.process.stringToProcess

object TSM {

  def main(args: Array[String]): Unit = {

    val cityCoordinates: Vector[(String, (Double, Double))] = Vector(
      ("Frankfurt am Main", (50.11222, 8.68194)),
      ("Berlin", (52.52222, 13.29750)),
      ("Leipzig", (51.340333, 12.37475)),
      ("München", (48.137222, 11.575556)),
      ("New York", (40.712778, -74.005833)),
      ("Hamburg", (53.550556, 9.993333)),
      ("Dresden", (51.049259, 13.73836)),
      ("Halle", (51.482778, 11.97)),
      ("Potsdam", (52.395833, 13.061389)),
      ("Erfurt", (50.978056, 11.029167)),
      ("London", (51.50939, -0.11832)),
      ("Tokio", (35.683889, 139.774444)),
      ("Kapstadt", (-33.922667, 18.416689)),
      ("Stuttgart", (48.775556, 9.182778)))

    val citys: Range = (0 until cityCoordinates.size)
    val distances: IndexedSeq[IndexedSeq[Double]] = calcDistances(cityCoordinates)

    val algorithm: Vector[((Range, IndexedSeq[IndexedSeq[Double]]) => (IndexedSeq[Int], Double), String)] =
      Vector( /*(BFS, "BFS"), (BFSParallel, "BFSP"), (BAB, "BAB"), (BABG, "BABG"), (DynRek, "DynRek"), (DynRekPar, "DynRekPar"),*/ (Dyn, "Dyn"), (DynPar, "DynPar"))

    val text = "#Laufzeiten für die Anzahl von enthaltenden Städten\n" +
      "#Städte Minimum Maximum Durschnitt Abweichung\n"
    var fileAlgoNames: Vector[(String, String)] = Vector()
    val dir = "bin/"

    algorithm.foreach { x =>
      val target = Paths get (dir + x._2 + "_" + System.currentTimeMillis() + ".dat");
      val file = Files createFile (target);
      Files.write(file, text.getBytes, StandardOpenOption.WRITE);
      fileAlgoNames = fileAlgoNames :+ (file.getFileName().toString(), x._2)
      (5 until citys.size).foreach { y =>
        println(y + 1 + " Städte")
        val res = calcResuslts(measure(30, citys.take(y), cityCoordinates, distances, x._1))
        println("Maximal: " ++ res._1)
        println("Minimal: " ++ res._2)
        println("Durchschnitt: " ++ res._3)
        println("Abweichung: " ++ res._4 + "\n\n")
        val line = "" + y + " " + res._1 + " " + res._2 + " " + res._3 + " " + res._4 + "\n"
        Files.write(file, line.getBytes(), StandardOpenOption.APPEND);
      }
      file.normalize()
    }
    produceGraphics(cityCoordinates.size, fileAlgoNames, dir)
  }

  def produceGraphics(citySize: Int, filenames: Vector[(String, String)], dir: String): Unit = {

    val source = Paths get ("src/project/" + "template.plt")
    val content = Files.readAllBytes(source)
    val destination = Paths get (dir + "plot.plt")
    Files.deleteIfExists(destination)
    Files.createFile(destination)

    var i = 0;
    val cmd = "set output \"" + dir + "res-" + System.currentTimeMillis() + ".png\"\n" +
      "set xrange [ 4.00000 : " + citySize + " ] noreverse nowriteback\n" +
      "plot " +
      filenames.foldLeft("") { (y, x) =>
        i += 1;
        y + "\'" + dir + x._1 + "\' using 1:4 lt " + i + " title \'" + x._2 + "\', '' using 1:4:5 w e lt " + i + " notitle ,"
      }
    Files.write(destination, content ++ cmd.getBytes(), StandardOpenOption.APPEND);
    "gnuplot " + dir + "plot.plt" !;
    Files.deleteIfExists(destination)
    Files deleteIfExists (Paths get ("fit.log"))
  }

  def measure[A](count: Int, citys: Range, cityCoordinates: Vector[(String, (Double, Double))], distances: IndexedSeq[IndexedSeq[Double]], f: (Range, IndexedSeq[IndexedSeq[Double]]) => (IndexedSeq[Int], Double)): ParSeq[Long] = {

    var measurements = ParSeq[Long]()

    System.gc()

    print("Round: ")

    (1 to count).foreach { i =>
      print(i + " ")
      val start = System.currentTimeMillis()
      val path = f(citys, distances)
      val stop = System.currentTimeMillis()
      measurements :+= (stop - start)
      if (i == count) {
        println("\nStrecke: " + path._1.map(x => cityCoordinates(x)._1) + "\nUmfang: " + path._2)
      }
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

  def calcDistance(city1: (Double, Double), city2: (Double, Double)): Double = {
    /*
   * Quelle: http://www.koordinaten.de/informationen/formel.shtml
   * Long, Lang in Grad
   */

    if (city1 == city2)
      0.0
    val city1B = (city1._1 / 180 * Pi, city1._2 / 180 * Pi)
    val city2B = (city2._1 / 180 * Pi, city2._2 / 180 * Pi)
    acos(sin(city1B._1) * sin(city2B._1) + cos(city1B._1) * cos(city2B._1) * cos(city1B._2 - city2B._2)) * 6378.137
  }

  def calcDistances(citys: IndexedSeq[(String, (Double, Double))]): IndexedSeq[IndexedSeq[Double]] = {

    var distances: IndexedSeq[IndexedSeq[Double]] = Vector()
    citys.foreach { city =>
      var dist: IndexedSeq[Double] = Vector()
      for (i <- 0 until citys.size) {
        dist = dist :+ calcDistance(city._2, citys(i)._2)
      }
      distances = distances :+ dist
    }
    distances
  }

  def BFS(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    citys.tail.permutations.toArray
      .map(citys.head +: _)
      .map(x => (x, x
        .foldLeft((x.last, 0.0))((res, value) => (value, distances(res._1)(value) + res._2))._2))
      .minBy(_._2)
  }

  def BFSParallel(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    citys.tail.permutations.toParArray
      .map(citys.head +: _)
      .map(x => (x, x
        .foldLeft((x.last, 0.0))((res, value) => (value, distances(res._1)(value) + res._2))._2))
      .minBy(_._2)

  }

  def BAB(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    var solution: (IndexedSeq[Int], Double) = (Vector(), Double.MaxValue)

    def BranchAndBound(citys: IndexedSeq[Int], path: (IndexedSeq[Int], Double), distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {
      if (citys.isEmpty) {
        (path._1, path._2 + distances(path._1.last)(path._1.head))
      } else {
        citys
          .map(x => (path._1 :+ x, distances(path._1.last)(x) + path._2))
          .foreach { alternative =>
            if (alternative._2 < solution._2) {
              val help: (IndexedSeq[Int], Double) = BranchAndBound(citys.filterNot(alternative._1 contains (_)), alternative, distances)
              if (help._2 < solution._2)
                solution = help
            }
          }
        solution
      }
    }

    BranchAndBound(citys.tail, (Vector(citys.head), 0.0), distances)
  }

  def BABG(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    var solution: (IndexedSeq[Int], Double) = (Vector(), Double.MaxValue)

    def BranchAndBoundGreedy(citys: IndexedSeq[Int], path: (IndexedSeq[Int], Double), distances: IndexedSeq[IndexedSeq[Double]], depth: Int): (IndexedSeq[Int], Double) = {
      if (citys.isEmpty) {
        (path._1, path._2 + distances(path._1.last)(path._1.head))
      } else {
        citys
          .map(x => (path._1 :+ x, distances(path._1.last)(x) + path._2))
          .sortBy(x => x._2)
          .foreach { alternative =>
            if (alternative._2 < solution._2) {
              val help: (IndexedSeq[Int], Double) = BranchAndBoundGreedy(citys.filterNot(alternative._1 contains (_)), alternative, distances, depth + 1)
              if (help._2 < solution._2)
                solution = help
            }
          }
        solution
      }
    }

    BranchAndBoundGreedy(citys.tail, (Vector(citys.head), 0.0), distances, 0)
  }

  /*
   * Quelle: http://stubber.math-inf.uni-greifswald.de/bioinformatik/folien07/Alg_Folien515-537.pdf
   * Seite 15
  */
  def DynRek(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    def g(city: Int, rest: IndexedSeq[Int]): (IndexedSeq[Int], Double) = {
      if (rest.isEmpty) {
        (Vector(), distances(city)(0))
      } else {
        rest.map { ncity =>
          val res = g(ncity, rest.filterNot(_ == ncity))
          (ncity +: res._1, distances(city)(ncity) + res._2)
        }.minBy(_._2)
      }
    }

    //start with first city
    val res = g(citys.head, citys.filterNot(_ == citys.head))
    (citys.head +: res._1, res._2)
  }

  def DynRekPar(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    def g(city: Int, rest: IndexedSeq[Int]): (IndexedSeq[Int], Double) = {
      if (rest.isEmpty) {
        (Vector(), distances(city)(0))
      } else {
        rest.par.map { ncity =>
          val res = g(ncity, rest.filterNot(_ == ncity))
          (ncity +: res._1, distances(city)(ncity) + res._2)
        }.minBy(_._2)
      }
    }

    //start with first city
    val res = g(citys.head, citys.filterNot(_ == citys.head))
    (citys.head +: res._1, res._2)
  }

  def Dyn(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    var help: HashMap[(Int, Set[Int]), (IndexedSeq[Int], Double)] = HashMap()

    def g(i: Int, S: Set[Int], citys: Set[Int]): (IndexedSeq[Int], Double) = {
      if (S.isEmpty)
        return (IndexedSeq(i), distances(i)(0))
      else {
        return S.map { x =>
          val lhelp = help(x, S - x)
          (i +: lhelp._1, distances(i)(x) + lhelp._2)
        }.minBy(_._2)
      }
    }

    val cCitys = citys.tail.toSet
    for (k <- 0 until citys.size) {
      cCitys.subsets(k).foreach { x =>
        cCitys.foreach { y =>
          if (!x.contains(y))
            help = help + (((y, x), g(y, x, cCitys)))
        }
      }
    }

    return g(0, cCitys, cCitys)
  }

  def DynPar(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    var help: HashMap[(Int, Set[Int]), (IndexedSeq[Int], Double)] = HashMap()

    def g(i: Int, S: Set[Int], citys: Set[Int]): (IndexedSeq[Int], Double) = {
      if (S.isEmpty)
        return (IndexedSeq(i), distances(i)(0))
      else {
        return S.map { x =>
          val lhelp = help(x, S - x)
          (i +: lhelp._1, distances(i)(x) + lhelp._2)
        }.minBy(_._2)
      }
    }

    val cCitys = citys.tail.toSet

    for (k <- 0 until citys.size) {
      val durchlauf: ParArray[ParSet[((Int, Set[Int]), (IndexedSeq[Int], Double))]] = cCitys.subsets(k).toParArray.map { x =>
        cCitys.par.map { y =>
          if (!x.contains(y))
            ((y, x), g(y, x, cCitys))
          else
            ((-1, Set(1)), (IndexedSeq(1), 0.0))
        }
      }
      for (a <- durchlauf.seq; b <- a.seq) { help = help + b }
    }

    return g(0, cCitys, cCitys)
  }

}
