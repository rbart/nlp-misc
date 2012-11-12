package edu.washington.cs.knowitall.nlp.util.relontology

sealed abstract class WordNetClass(val string: String)

object WordNetClass {
  val allClasses = Seq(
      Abstraction, 
      Animal,
      Artifact,
      Communication,
      Group,
      HumanActivity,
      Knowledge,
      Location,
      Number,
      Occurrence,
      Organization,
      Person,
      PhysicalEntity,
      TimePeriod,
      Unit,
      OtherNoun)
  
  val classNameMap: Map[String, WordNetClass] = allClasses.map(c => (c.string, c)).toMap
  
  // Returns None if input doesn't match any class.
  // Returns OtherNoun only if input equals "other_noun"
  def fromString(string: String): Option[WordNetClass] = classNameMap.get(string)
}

case object Abstraction extends WordNetClass("abstraction[n6]")
case object Animal extends WordNetClass("animal[n1]")
case object Artifact extends WordNetClass("artifact[n1]")
case object Communication extends WordNetClass("communication[n2]")
case object Group extends WordNetClass("group[n1]")
case object HumanActivity extends WordNetClass("human_activity[n1]")
case object Knowledge extends WordNetClass("knowledge[n1]")
case object Location extends WordNetClass("location[n1]")
case object Number extends WordNetClass("number[n2]")
case object Occurrence extends WordNetClass("occurrence[n1]")
case object Organization extends WordNetClass("organization[n1]")
case object Person extends WordNetClass("person[n1]")
case object PhysicalEntity extends WordNetClass("physical_entity[n1]")
case object TimePeriod extends WordNetClass("time_period[n1]")
case object Unit extends WordNetClass("unit[n1]")

// the catch-all:
case object OtherNoun extends WordNetClass("other_noun")