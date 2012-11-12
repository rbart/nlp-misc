package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.tool.postag.PostaggedToken

// represents a CLEAN entailment... 
case class CleanEntailment(val left: String, val right: String, val leftRev: Boolean, val rightRev: Boolean)

object CleanEntailment {
  def fromString(string: String): Option[CleanEntailment] = {
    string.split("\t") match {
      case Array(rawLeft, rawRight, _*) => {
    	
    	val leftRev = rawLeft.endsWith("@R@")
    	val left = if (leftRev) rawLeft.dropRight(3) else rawLeft
    	
    	val rightRev = rawRight.endsWith("@R@")
    	val right = if (rightRev) rawRight.dropRight(3) else rawRight
    	Some(CleanEntailment(left, right, leftRev, rightRev))
      }
      case _ => { System.err.println("Couldn't parse:%s".format(string)); None }
    }
  }
}

/**
 * A helper class for performing lookups against Berant's CLEAN entailments.
 */
class CleanUtils(val entailments: Iterable[CleanEntailment]) {

  def getCleanEntailments(matchString: String): Seq[CleanEntailment] = {
    
    entailments.filter { ent =>
      // want not reversed (or both reversed, equivalently)
      val revOk = (ent.leftRev == ent.rightRev) 
      def matches = (ent.left.equalsIgnoreCase(matchString) || ent.right.equalsIgnoreCase(matchString))
      revOk && matches
    } toSeq
  }
}

object CleanUtils {
  
  import edu.washington.cs.knowitall.common.Resource.using
  
  // only good on CSE filesystem...
  val defaultHtlFile = "/projects/WebWare9/CLEAN_data/Resource0812/reverb_global_clsf_all_htl_lambda0.05.txt"
  val defaultTncfFile = "/projects/WebWare9/CLEAN_data/Resource0812/reverb_global_clsf_all_tncf_lambda_0.1.txt"
  
  lazy val defaultHtl = new CleanUtils(loadCleanFile(defaultHtlFile))
  lazy val defaultTncf = new CleanUtils(loadCleanFile(defaultTncfFile))
    
  def loadCleanFile(fileName: String): Iterable[CleanEntailment] = {
    
    using(scala.io.Source.fromFile(fileName)) { source =>
      source.getLines flatMap CleanEntailment.fromString toList // toList to guarantee full materialization... is there a better way?
    }
  }
}

object CleanUtilsTest {
  
  import scopt.OptionParser
  
  def main(args: Array[String]): Unit = {
    
    var searchString = ""
      
    val parser = new OptionParser("CleanUtilsTest") {
      arg("searchString", "string to search clean for", { str => searchString = str })
    }
    
    if (!parser.parse(args)) return
    
    println("HTL Entailments:")
    CleanUtils.defaultHtl.getCleanEntailments(searchString) foreach { ent => 
      println("%s\t%s".format(ent.left, ent.right))
    }
    
    println("\nTNCF Entailments:")
    CleanUtils.defaultTncf.getCleanEntailments(searchString) foreach { ent => 
      println("%s\t%s".format(ent.left, ent.right))
    }
  }
}
