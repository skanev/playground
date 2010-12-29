import sbt._

class ExpressionsProject(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.1"
  val scalacheck = "org.scala-tools.testing" % "scalacheck_2.7.7" % "1.6"
}
