import java.io.{FileInputStream, FileOutputStream, File}

import com.lambdaworks.jacks.JacksMapper
import org.apache.commons.io.IOUtils

import scala.sys.process._
import scala.util.Random

object csm {

  def main(args: Array[String]) {

    println("Finding files under version control.")
    val files = findFiles("*.java")
//    val files = Array("a", "b", "c", "d", "e", "f", "g") //findFiles("*.java")
    println("Finding commits for each file.")
    val fileCommits = findCommits(files)
//    val fileCommits = Map(
//        "a" -> Set("abc", "ad"),
//        "b" -> Set("abc"),
//        "c" -> Set("abc", "cg"),
//        "d" -> Set("ad", "defg"),
//        "e" -> Set("defg"),
//        "f" -> Set("defg"),
//        "g" -> Set("defg", "cg")
//      )

    def printClusters(cls: Vector[Set[Int]]) {
      println
      cls.filter(_.size > 1).foreach((cli) => {
        val clFiles = cli.map(files(_))
        val commitsPerFile: Set[Set[String]] = clFiles.map(fileCommits(_))
        val combinedCommits = commitsPerFile.foldLeft(Set.empty[String])((x, commits) => {
          x union commits
        })
        val commonCommits = commitsPerFile.foldLeft(combinedCommits)((x, commits) => {
          x intersect commits
        })

        println("Clustered files:")
        println(clFiles.mkString("\n"))
        println
        println("Commits combined:")
        combinedCommits.foreach((commit) => {
          val filesWithCommit = clFiles.filter(fileCommits(_).contains(commit)).mkString(", ")
          println(f"${commit.split(" ")(0)} ($filesWithCommit)")
        })
        println
        println("Commits in common:")
        println(commonCommits.mkString("\n"))
        println
        println
      })
    }

    println("Creating DSM")
    val dsm: Vector[Vector[Double]] = createDsm(files, fileCommits)
    val allElements: Set[Int] = files.indices.toSet

    println("Creating initial clusters")
    val initialCls: Set[Set[Int]] = createInitialClusters(allElements, dsm)
    printClusters(initialCls.toVector)

    println("Creating secondary clusters")
    var secondaryCls: Vector[Set[Int]] = createSecondaryClusters(allElements, initialCls)
    printClusters(secondaryCls)

    def dependency(secV: Int, secCl: Set[Int]) = {
      val dep = secCl.map((y) => dsm(secV)(y)).sum / secCl.size
      if (dep != 1.0) dep
      else 1 - 1/(2.0 * secCl.size)
    }

    def bestFit(secondaryCls: Vector[Set[Int]], secCl1: Set[Int], j: Int): Set[Int] = {
      val bestTuple = secondaryCls.map((secCl2) => (dependency(j, secCl2), secCl2)).maxBy(_._1)
      if ((bestTuple._2 -- secCl1).isEmpty || bestTuple._1 <= dependency(j, secCl1 - j)) secCl1
      else bestTuple._2
    }


    println("Creating final clusters")
    var done = false
    var changes = 0
    while (!done && changes < 10000) {
      var toCl = Set.empty[Int]
      var variable = Option.empty[Int]

      secondaryCls.find((secCl1) => {
        variable = secCl1.find((j) => {
          toCl = bestFit(secondaryCls, secCl1, j)
          toCl.intersect(secCl1).isEmpty
        })
        variable.nonEmpty
      }) match {
        case Some(fromCl) =>
          secondaryCls = secondaryCls.updated(secondaryCls.indexOf(fromCl), fromCl - variable.get)
          secondaryCls = secondaryCls.updated(secondaryCls.indexOf(toCl), toCl + variable.get)
          done = false
          changes += 1
        case None => done = true
      }
    }

    printClusters(secondaryCls)




//    for ((row, i) <- csm.sliding(files.length, files.length).zipWithIndex) {
//      val filename = files(i)
//      println(f"$filename,${row.mkString(",")}")
//    }

  }

  def createSecondaryClusters(allElements: Set[Int], initialCls: Set[Set[Int]]): Vector[Set[Int]] = {
    var candidates: Set[Int] = allElements
    var secondaryCls: Vector[Set[Int]] = Vector.empty

    while (candidates.size > 0) {
      val i = randomElement(candidates)
      var secondaryCl = Set(i)

      secondaryCl ++= (candidates -- secondaryCl).filter(coOccurrenceRatio(_, i, initialCls) >= 0.5)

      val validCls = initialCls.filter((cl) => cl.intersect(secondaryCl).size >= 0.5 * secondaryCl.size)

      secondaryCl ++= (secondaryCl - i).flatMap((k) => {
        (candidates -- secondaryCl).filter(coOccurrenceRatio(_, k, validCls) >= 0.5)
      })

      secondaryCls = secondaryCls :+ secondaryCl
      candidates --= secondaryCl
    }
    secondaryCls
  }

  def randomElement(elements: Set[Int]): Int = elements.toVector(new Random().nextInt(elements.size))

  def coOccurrenceRatio(j: Int, i: Int, clusters: Set[Set[Int]]): Double = {
    val noOfClustersWithBoth = clusters.count((cluster) => cluster.contains(i) && cluster.contains(j))
    val noOfClustersWithEither = clusters.count((cluster) => cluster.contains(i) || cluster.contains(j))
    1.0 * noOfClustersWithBoth / noOfClustersWithEither
  }

  def createInitialClusters(allElements: Set[Int], dsm: Vector[Vector[Double]]): Set[Set[Int]] = {

    def toCluster(i: Int): Set[Int] = {
      val group = allElements.filter(dsm(i)(_) > 0)
      reduce(group) + i
    }

    def reduce(group: Set[Int]): Set[Int] = {
      var gpx = group
      while (density(gpx) < 0.5 && weakVariables(gpx).nonEmpty) {
        gpx -= weakVariables(gpx)(0)
      }
      gpx
    }

    def sumOfCombinations(group: Set[Int]): Double = group.toVector.combinations(2).map((x) => { dsm(x(0))(x(1)) }).sum

    def density(group: Set[Int]):Double = (2 * sumOfCombinations(group)) / (group.size * group.size)

    def strength(a: Int, group: Set[Int]): Double = Stream.continually(a).zip(group).map((x) => dsm(x._1)(x._2)).sum

    def weakVariables(group: Set[Int]): Vector[Int] = {
      group.map((a) => (strength(a, group), a)).filter(_._1 < (group.size / 2)).toVector.sortBy(_._1).map(_._2)
    }

    allElements.map(toCluster)
  }

  def createDsm(files: Array[String], fileCommits: Map[String, Set[String]]): Vector[Vector[Double]] = {
    val dsm: Vector[Vector[Double]] = (for {
      a <- files
      b <- files
    } yield {
      if (a == b) 0
      else {
        val linkage = (fileCommits(a) intersect fileCommits(b)).size
        if (linkage == 0) 0.0
        else 1.0 //- 1 / linkage
      }
    }).toVector.sliding(files.length, files.length).toVector
    dsm
  }

  def findCommits(files: Array[String]): Map[String, Set[String]] = {
    val filesCommitsFile = new File("/Users/nch/Coding/csm/filesCommits.txt")
    if (filesCommitsFile.exists()) {
      val inputStream = new FileInputStream(filesCommitsFile)
      try {
        return JacksMapper.readValue[Map[String, Set[String]]](IOUtils.toString(inputStream))
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
        return filesCommits
      } finally {
        outputStream.close()
      }
    }
  }

  def findFiles(arg: String): Array[String] = {
    val filesFile = new File("/Users/nch/Coding/csm/files.txt")
    if (filesFile.exists()) {
      val inputStream = new FileInputStream(filesFile)
      try {
        return JacksMapper.readValue[Array[String]](IOUtils.toString(inputStream))
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
        return files
      } finally {
        outputStream.close()
      }
    }
  }
}
//csm.main(args)

//#!/bin/sh
//exec scala "$0" "$@"
//!#
