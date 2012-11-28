package edu.washington.cs.knowitall.nlp.util

import scopt.OptionParser
import scala.io.Source
import java.io.PrintStream

import edu.washington.cs.knowitall.common.Resource.using
// filters output from /browser-hadoop/src/main/scala/edu/washington/cs/knowitall/browser/hadoop/scoobi/ArgPairExtractor.scala
// using standard input as the default input source, or an optional file.
// output goes to stdout by def., file optionally.
// The idea is to produce something suitable for sorting via FileSorter and then use by SortedFileMap
// 
object ArgPairFilter {

  def main(args: Array[String]): Unit = {

    var input = ""
    var output = ""
    var minPairs = 10

    val parser = new OptionParser("ArgPairFilter") {
      opt("in", "input file (def. stdin)", { input = _ })
      opt("out", "output file (def. stdout)", { output = _ })
      intOpt("minPairs", "minimum arg pairs to pass filter", { minPairs = _ })
    }

    if (!parser.parse(args)) {
      return ;
    }

    def inSrc = if (input.isEmpty()) Source.stdin else Source.fromFile(input)
    def outSrc = if (output.isEmpty()) System.out else new PrintStream(output)

    // spit into rel and argpairs (break off the first column), None if invalid
    def parseLine(line: String): Option[(String, String)] = {
      val indexToSplit = line.indexWhere(_ == '\t')
      if (indexToSplit == -1) return None
      else {
        try {
          return Some(line.substring(0, indexToSplit - 1), line.substring(indexToSplit + 1))
        } catch { 
          case e => {
            System.err.println("Screwed up line.substring...\nline=%s\nindexToSplit=%s\nLength=%d".format(line, indexToSplit, line.length))
            None
          }
        }
      }
    }
    
    def backToString(pair: (String, String)): String = "%s\t%s".format(pair._1, pair._2)

    using(inSrc) { in =>
      using(outSrc) { out =>
        in.getLines flatMap parseLine filter { case (rel, argPairs) =>
          argPairs.count(_ == '\t') >= minPairs - 1
        } map backToString foreach out.println
      } 
    }

  }

}