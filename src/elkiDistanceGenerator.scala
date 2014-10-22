import java.io._

import com.lambdaworks.jacks.JacksMapper
import org.apache.commons.io.IOUtils

import scala.collection.immutable.IndexedSeq
import scala.sys.process._
import scala.util.Random

object elkiDistanceGenerator {

  def main(args: Array[String]) {

    val files = findFiles("/Users/nch/Coding/dsm/fitnesse/src/fitnesse/*.java")
    val fileCommits = findCommits(files)
    writeElkiFiles(files)
    writeElkiDistance(files, fileCommits)
  }

  def normalize(linkage: Int, dMin: Int, dMax: Int): Double = {
    1.0d - 1.0d * (linkage - dMin) / (dMax - dMin)
  }

  def writeElkiDistance(files: Array[String], fileCommits: Map[String, Set[String]]) = {
    val elkiDistanceFile = new File("fitnesse.elki")
    if (elkiDistanceFile.createNewFile()) {
      val writer = new BufferedWriter(new FileWriter(elkiDistanceFile))

      val allLinkage: Seq[(Int, Int, Int)] = (0 until files.size).combinations(2).map((pair) => {
        val i = pair(0)
        val j = pair(1)
        val linkage = (fileCommits(files(i)) intersect fileCommits(files(j))).size
        (i, j, linkage)
      }).toSeq

      val dMin = allLinkage.minBy(_._3)._3
      val dMax = allLinkage.maxBy(_._3)._3

      println(f"$dMin $dMax")

      val normalizedLinkage = allLinkage.map((d) => {(d._1, d._2, normalize(d._3, dMin, dMax))})
      normalizedLinkage.foreach((d) => {
        if (d._2 == (d._1+1)) {
          writer.write(f"${d._1} ${d._1} 1.00000")
          writer.newLine()
        }
        writer.write(f"${d._1} ${d._2} ${d._3}%1.5f")
        writer.newLine()
      })
      writer.close()

    }
  }

  def writeElkiFiles(files: Array[String]) = {
    val elkiDistanceFile = new File("fitnesse.elkidb")
    if (elkiDistanceFile.createNewFile()) {
      val writer = new BufferedWriter(new FileWriter(elkiDistanceFile))

      files.zipWithIndex.foreach((f) => {
        writer.write(f"${f._2} ${f._1}")
        writer.newLine()
      })
      writer.close()
    }
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
