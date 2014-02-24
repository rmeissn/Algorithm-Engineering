package project

import scala.collection.immutable.Vector
import scala.collection.mutable.ArraySeq
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.traversable2ops
import scala.math.Pi
import scala.math.acos
import scala.math.cos
import scala.math.sin
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption
import java.io.IOException
import scala.sys.process.ProcessBuilder
import scala.sys.process.Process

object TSM {

  def main(args: Array[String]): Unit = {

    lazy val cityCoordinates: Vector[(String, (Double, Double))] = Vector(
      ("Frankfurt am Main", (50.11222, 08.68194)),
      ("Berlin", (52.52222, 13.29750)),
      ("Leipzig", (51.340333, 12.37475)),
      ("München", (48.137222, 11.575556)),
      ("New York", (40.712778, -74.005833)),
      ("Hamburg", (53.550556, 9.993333)),
      ("Dresden", (51.049259, 13.73836)),
      ("Halle", (51.482778, 11.97)),
      //      ("Potsdam", (52.395833, 13.061389)),
      //      ("Erfurt", (50.978056, 11.029167)),
      //      ("London", (51.50939, -0.11832)),
      ("Stuttgart", (48.775556, 9.182778)))

    lazy val citys: Range = (0 until cityCoordinates.size)
    lazy val distances: IndexedSeq[IndexedSeq[Double]] = calcDistances(cityCoordinates)

    lazy val algorithm: Vector[((Range, IndexedSeq[IndexedSeq[Double]]) => (IndexedSeq[Int], Double), String)] =
      Vector( (BFS, "BFS")/*, (BFSParallel, "BFSP") , (BAB, "BAB"), (BABG, "BABG") */, (DynRek,"DynRek") )

    lazy val text = "#Laufzeiten für die Anzahl von enthaltenden Städten\n" +
      "#Städte Minimum Maximum Durschnitt Abweichung\n"
    var fileAlgoNames: Vector[(String, String)] = Vector()
    lazy val dir = "/home/roy/AlgoTest/"

    algorithm.foreach { x =>
      lazy val target = Paths get (dir + x._2 + "_" + System.currentTimeMillis() + ".dat");
      lazy val file = Files createFile (target);
      Files.write(file, text.getBytes, StandardOpenOption.WRITE);
      fileAlgoNames = fileAlgoNames :+ (file.getFileName().toString(), x._2)
      (5 until citys.size).foreach { y =>
        println(y + 1 + " Städte")
        lazy val res = calcResuslts(measure(30, citys.take(y), cityCoordinates, distances, x._1))
        println("Maximal: " ++ res._1)
        println("Minimal: " ++ res._2)
        println("Durchschnitt: " ++ res._3)
        println("Abweichung: " ++ res._4 + "\n\n")
        lazy val line = "" + y + " " + res._1 + " " + res._2 + " " + res._3 + " " + res._4 + "\n"
        Files.write(file, line.getBytes(), StandardOpenOption.APPEND);
      }
      file.normalize()
    }
    produceGraphics(cityCoordinates.size, fileAlgoNames, dir)
  }

  def produceGraphics(citySize: Int, filenames: Vector[(String, String)], dir: String): Unit = {

    val source = Paths get (dir + "template.plt")
    val content = Files.readAllBytes(source)
    val destination = Paths get (dir + "plot.plt")
    Files.deleteIfExists(destination)
    Files.createFile(destination)

    val cmd = "set output \"" + dir + "res-" + System.currentTimeMillis() + ".png\"\n" +
      "set xrange [ 4.00000 : " + citySize + " ] noreverse nowriteback\n" +
      "fit f(x) '" + dir + filenames(0)._1 + "' using 1:4:5 via a,b\n" +
      "fit g(x) '" + dir + filenames(1)._1 + "' using 1:4:5 via c,d\n" +
      "plot '" + dir + filenames(0)._1 + "' using 1:4:5 w e notitle , f(x) w l title '" + filenames(0)._2 + "','" + dir + filenames(1)._1 + "' using 1:4:5 w e notitle , g(x) w l title '" + filenames(1)._2 + "'"
    //    val cmd = pre + filenames.foldLeft("") { (y,x) =>
    //      y + "\'" + dir + x._1 + "\' using 1:4 w lp title \'" + x._2 + "\', '' using 1:4:5 w e notitle,"
    //      "'/home/roy/AlgoTest/BAB_1389872973200.dat' using 1:4:5 w e notitle , f(x) w l title 'BAB'"
    //    }
    Files.write(destination, content ++ cmd.getBytes(), StandardOpenOption.APPEND);
    Process("gnuplot " + dir + "plot.plt").run()
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
    lazy val smallseq = seq.drop(10)
    lazy val middle = (smallseq.sum / smallseq.length)
    return ((smallseq.max).toString, (smallseq.min).toString, middle.toString,
      Math.sqrt(((smallseq.map(x => Math.pow(x - middle, 2))).sum / smallseq.length)).toString)
  }

  /*
   * Quelle: http://www.koordinaten.de/informationen/formel.shtml
   * Long, Lang in Grad
   */
  def calcDistance(city1: (Double, Double), city2: (Double, Double)): Double = {

    if (city1 == city2)
      0.0
    lazy val city1B = (city1._1 / 180 * Pi, city1._2 / 180 * Pi)
    lazy val city2B = (city2._1 / 180 * Pi, city2._2 / 180 * Pi)
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

    citys.tail.permutations
      .map(citys.head +: _)
      .map(x => (x, x
        .foldLeft((x.last, 0.0))((res, value) => (value, distances(res._1)(value) + res._2))._2))
      .minBy(_._2)
  }

  def BFSParallel(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    citys.tail.permutations
      .map(citys.head +: _).toParArray
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
              lazy val help: (IndexedSeq[Int], Double) = BranchAndBound(citys.filterNot(alternative._1 contains (_)), alternative, distances)
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
              lazy val help: (IndexedSeq[Int], Double) = BranchAndBoundGreedy(citys.filterNot(alternative._1 contains (_)), alternative, distances, depth + 1)
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
    lazy val res = g(citys.head, citys.filterNot(_ == citys.head))
    (citys.head +: res._1, res._2)
  }
  
  def Dyn(citys: Range, distances: IndexedSeq[IndexedSeq[Double]]): (IndexedSeq[Int], Double) = {

    var distances: ArraySeq[ArraySeq[Int]] = ArraySeq()
    //fill array with values
    //find lowest value
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
    lazy val res = g(citys.head, citys.filterNot(_ == citys.head))
    (citys.head +: res._1, res._2)
  }

}
