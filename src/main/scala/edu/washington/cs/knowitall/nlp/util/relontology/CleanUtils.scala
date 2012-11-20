package edu.washington.cs.knowitall.nlp.util.relontology

import edu.washington.cs.knowitall.tool.postag.PostaggedToken

// represents a record from one of the CLEAN files 
case class CleanRecord(val left: String, val right: String, val leftRev: Boolean, val rightRev: Boolean)

object CleanRecord {
  def fromString(string: String): Option[CleanRecord] = {
    string.split("\t") match {
      case Array(rawLeft, rawRight, _*) => {
    	
    	val leftRev = rawLeft.endsWith("@R@")
    	val left = if (leftRev) rawLeft.dropRight(3) else rawLeft
    	
    	val rightRev = rawRight.endsWith("@R@")
    	val right = if (rightRev) rawRight.dropRight(3) else rawRight
    	Some(CleanRecord(left, right, leftRev, rightRev))
      }
      case _ => { System.err.println("Couldn't parse:%s".format(string)); None }
    }
  }
}

/**
 * A helper class for performing lookups against Berant's CLEAN entailments.
 */
class CleanUtils(val entailments: Iterable[CleanRecord]) {

  def getCleanEntailments(matchString: String): Seq[ClEntailment] = {
    
    entailments.filter({ ent => // want not reversed (or both reversed, equivalently)
      val revOk = (ent.leftRev == ent.rightRev) 
      def matches = ent.left.equalsIgnoreCase(matchString)
      revOk && matches;
    }).toSeq.map(cr => ClEntailment((cr.right)))
  }
  
  def getCleanTroponyms(matchString: String): Seq[ClTroponym] = {
    
    entailments.filter({ ent => // want not reversed (or both reversed, equivalently)
      val revOk = (ent.leftRev == ent.rightRev) 
      def matches = ent.right.equalsIgnoreCase(matchString)
      revOk && matches;
    }).toSeq.map(cr => ClTroponym((cr.left)))
  }
  
  def getCleanNyms(matchString: String): Seq[Nym] = getCleanEntailments(matchString) ++ getCleanTroponyms(matchString)
}

object CleanUtils {
  
  import edu.washington.cs.knowitall.common.Resource.using
  
  // only good on CSE filesystem...
  val defaultHtlFile = "/projects/WebWare9/CLEAN_data/Resource0812/reverb_global_clsf_all_htl_lambda0.05.txt"
  val defaultTncfFile = "/projects/WebWare9/CLEAN_data/Resource0812/reverb_global_clsf_all_tncf_lambda_0.1.txt"
  
  lazy val defaultHtl = new CleanUtils(loadCleanFile(defaultHtlFile))
  lazy val defaultTncf = new CleanUtils(loadCleanFile(defaultTncfFile))
    
  lazy val defaultBoth = new CleanUtils(loadCleanFile(defaultHtlFile) ++ loadCleanFile(defaultTncfFile))
  
  def loadCleanFile(fileName: String): Iterable[CleanRecord] = {
    
    using(scala.io.Source.fromFile(fileName)) { source =>
      source.getLines flatMap CleanRecord.fromString toList // toList to guarantee full materialization... is there a better way?
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
      println(ent)
    }
    println("HTL Troponyms:")
    CleanUtils.defaultHtl.getCleanTroponyms(searchString) foreach { ent => 
      println(ent)
    }
    
    println("\nTNCF Entailments:")
    CleanUtils.defaultTncf.getCleanEntailments(searchString) foreach { ent => 
      println(ent)
    }
    println("\nTNCF Troponyms:")
    CleanUtils.defaultTncf.getCleanTroponyms(searchString) foreach { ent => 
      println(ent)
    }
  }
}
