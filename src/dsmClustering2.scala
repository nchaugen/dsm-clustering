import java.io.{FileInputStream, FileOutputStream, File}

import com.lambdaworks.jacks.JacksMapper
import org.apache.commons.io.IOUtils

import scala.collection.immutable.IndexedSeq
import scala.sys.process._
import scala.util.Random

object dsmClustering2 {

  val a: Double = 1.0 / 3 //weight of s1 errors
  val b: Double = 1.0 / 3 //weight of s2 errors
  val c: Double = 1 - a - b
  var log2nn = 0d
  var ax = 0d
  var bx = 0d
  var cx = 0d


  def main(args: Array[String]) {

    val files = findFiles("/Users/nch/Coding/dsm/fitnesse/src/fitnesse/*.java")
    val fileCommits = findCommits(files)
    val dsm: Vector[Vector[Double]] = createDsm(files, fileCommits)
    printDsm(dsm)

//        val dsm: Vector[Vector[Double]] = Vector(Vector(-1,  0,  0,  0,  1,  0,  1,  0,  0),
//                                              Vector( 0, -1,  1,  0,  0,  1,  0,  1,  0),
//                                              Vector( 0,  1, -1,  0,  0,  1,  0,  1,  0),
//                                              Vector( 0,  0,  0, -1,  0,  0,  0,  0,  0),
//                                              Vector( 1,  0,  0,  0, -1,  0,  1,  0,  0),
//                                              Vector( 0,  1,  1,  0,  0, -1,  0,  1,  0),
//                                              Vector( 1,  0,  0,  0,  1,  0, -1,  0,  0),
//                                              Vector( 0,  1,  1,  0,  0,  1,  0, -1,  0),
//                                              Vector( 0,  0,  0,  0,  0,  0,  0,  0, -1))

    val numNodes = dsm.size
    val numClusters = 20
    val sizeParentPopulation = 100
    val sizeChildPopulation = 100
    val sizeImmigrantPopulation = 200
    val numGenerations = 500

    log2nn = log2(numNodes)
    ax = a * (2 * log2nn + 1)
    bx = b * (2 * log2nn + 1)
    cx = c * log2nn


    println(f"DSM is $numNodes * $numNodes, looking for $numClusters clusters with l=$sizeParentPopulation, u=$sizeChildPopulation and i=$sizeImmigrantPopulation after $numGenerations generations")

    var parentPopulation = initParentPopulation(sizeParentPopulation, numClusters, dsm)

    (1 to numGenerations).foreach((i) => {

      val childPopulation = createChildPopulation(dsm, numClusters, parentPopulation, sizeChildPopulation)
      val immigrantPopulation = initParentPopulation(sizeImmigrantPopulation, numClusters, dsm)
      val population: Vector[(Double, Vector[Boolean])] = (parentPopulation ++ childPopulation ++ immigrantPopulation).sortBy(_._1)
      parentPopulation = population.take(sizeParentPopulation)

      println(f"$i: ${averageFdsm(population)}%1.4f (all), ${averageFdsm(parentPopulation)}%1.4f (parents)")
      printPopulation(parentPopulation.take(1), numNodes)
      println()
    })
  }

  def averageFdsm(pop: Vector[(Double, Vector[Boolean])]) = 1.0d * pop.map(_._1).sum / pop.size

  def printDsm(dsm: Vector[Vector[Double]]) = {
    dsm.foreach((dj) => println(dj.map((dij) => f"$dij%1.2f").mkString(" ")))
    println()
  }

  def printPopulation(epop: Vector[(Double, Vector[Boolean])], numNodes: Int) {
    def clusterToString: (Vector[Boolean]) => String = {
      "[" + _.zipWithIndex.filter(_._1).map(_._2).mkString(",") + "]"
    }

    epop.foreach((ec) => {
      print(f"${ec._1}%3.4f: ")
      //      print(ec._2.map((b) => {
      //        if (b) "1" else "0"
      //      }).mkString(" ") + " ")
      println(ec._2.sliding(numNodes, numNodes).map(clusterToString).mkString(","))
    })
  }

  def shouldCrossover(): Boolean = new Random().nextBoolean()

  def shouldMutate(): Boolean = new Random().nextInt(10) == 1

  def possiblyMutatedGene(gene: Boolean): Boolean = if (shouldMutate()) !gene else gene

  def offspring(dsm: Vector[Vector[Double]], numClusters: Int, mother: Vector[Boolean], father: Vector[Boolean]): Vector[(Double, Vector[Boolean])] = {
    val chrome = mother.zipWithIndex.map((x) => {
      if (shouldCrossover()) (x._1, father(x._2)) else (father(x._2), x._1)
    })
    val child1 = chrome.map((cs) => possiblyMutatedGene(cs._1)).toVector
    val child2 = chrome.map((cs) => possiblyMutatedGene(cs._2)).toVector

    Vector(
      (fdsm(dsm, numClusters, child1), child1),
      (fdsm(dsm, numClusters, child2), child2)
    )
  }

  def randomParent(parentPopulation: Vector[(Double, Vector[Boolean])]): Vector[Boolean] =
    parentPopulation(random(parentPopulation.size))._2

  def createChildPopulation(dsm: Vector[Vector[Double]], numClusters: Int, parentPopulation: Vector[(Double, Vector[Boolean])], sizeChildPopulation: Int): Vector[(Double, Vector[Boolean])] = {
    (for {
      c <- 0 until sizeChildPopulation / 2
    } yield offspring(dsm, numClusters, randomParent(parentPopulation), randomParent(parentPopulation))).flatten.toVector
  }

  def encodeChrome(numNodes: Int, numClusters: Int, clNodeTuples: IndexedSeq[(Int, Int)]): Vector[Boolean] = {
    (for {
      ci <- 0 until numClusters
      ni <- 0 until numNodes
    } yield clNodeTuples.contains(ci, ni)).toVector
  }

  def initParentPopulation(popSize: Int, numClusters: Int, dsm: Vector[Vector[Double]]): Vector[(Double, Vector[Boolean])] = {
    val numNodes = dsm.size

    def createParentChrome(): (Double, Vector[Boolean]) = {
      val chrome = encodeChrome(numNodes, numClusters, (0 until numNodes).map((n) => {
        (random(numClusters), n)
      }))
      (fdsm(dsm, numClusters, chrome), chrome)
    }

    (for {
      pop <- 0 until popSize
    } yield createParentChrome()).toVector
  }

  def fdsm(dsm: Vector[Vector[Double]], numClusters: Int, chrome: Vector[Boolean]): Double = {

    val numNodes = dsm.size
    val sumNumNodesInClusters = chrome.count((g) => g)

    val s: (Double, Double) = {
      val clusters: Vector[Vector[Boolean]] = chrome.sliding(numNodes, numNodes).toVector
      val bus: Vector[Boolean] = clusters(clusters.size - 1)

      def mismatch(i: Int, j: Int): (Double, Double) = {
        val d = dsm(i)(j)
        val dd = clusters.exists((cluster) => {
          cluster(i) && cluster(j)
        }) || bus(i) || bus(j)
//        val s1 = if (d == 0 && dd) 1 else 0
//        val s2 = if (d == 1 && !dd) 1 else 0
        val s1 = if (dd) 1.0 - d else 0d
        val s2 = if (!dd) 1.0 * d else 0d
        (s1, s2)
      }

      (for {
        i <- 0 until numNodes
        j <- 0 until numNodes
        if i != j
      } yield mismatch(i, j)).foldLeft((0d, 0d))((b, s) => {
        (b._1 + s._1, b._2 + s._2)
      })
    }

//    val log2nn = log2(numNodes)
//    val a: Double = 1.0 / 3 //weight of s1 errors
//    val b: Double = 1.0 / 3 //weight of s2 errors
//    val c: Double = 1 - a - b
//    val ax = a * (2 * log2nn + 1)
//    val bx = b * (2 * log2nn + 1)
//    val cx = c * log2nn

    cx * (numClusters + sumNumNodesInClusters) + ax * s._1 + bx * s._2
  }

  val lnOf2 = scala.math.log(2)

  // natural log of 2
  def log2(x: Double): Double = scala.math.log(x) / lnOf2

  def random(i: Int) = new Random().nextInt(i)

  def createDsm(files: Array[String], fileCommits: Map[String, Set[String]]): Vector[Vector[Double]] = {

    val dsmLinkage: Seq[Int] = (for {
      a <- files
      b <- files
    } yield {
      if (a == b) 0
      else (fileCommits(a) intersect fileCommits(b)).size
      }).toSeq

    val dMin = dsmLinkage.min
    val dMax = dsmLinkage.max

    val dsm = dsmLinkage.map((d) => {1.0d*(d-dMin)/(dMax-dMin)}).toVector.sliding(files.length, files.length).toVector
    dsm
  }

  def findCommits(files: Array[String]): Map[String, Set[String]] = {
    val filesCommitsFile = new File("filesCommits.txt")
    if (filesCommitsFile.exists()) {
      val inputStream = new FileInputStream(filesCommitsFile)
      try {
        JacksMapper.readValue[Map[String, Set[String]]](IOUtils.toString(inputStream))
      } finally {
        inputStream.close()
      }
    }
    else {
      val filesCommits = files.map(file => {
        val commits = f"git log --follow --oneline $file".!!.split("\\n").toSet
        file -> commits
      }).toMap
      filesCommitsFile.createNewFile()
      val outputStream = new FileOutputStream(filesCommitsFile)
      try {
        IOUtils.write(JacksMapper.writeValueAsString(filesCommits), outputStream)
        filesCommits
      } finally {
        outputStream.close()
      }
    }
  }

  def findFiles(arg: String): Array[String] = {
    val filesFile = new File("files.txt")
    if (filesFile.exists()) {
      val inputStream = new FileInputStream(filesFile)
      try {
        JacksMapper.readValue[Array[String]](IOUtils.toString(inputStream))
      } finally {
        inputStream.close()
      }
    }
    else {
      val files = f"git ls-files $arg".!!.split("\\n")
      filesFile.createNewFile()
      val outputStream = new FileOutputStream(filesFile)
      try {
        IOUtils.write(JacksMapper.writeValueAsString(files), outputStream)
        files
      } finally {
        outputStream.close()
      }
    }
  }
}
