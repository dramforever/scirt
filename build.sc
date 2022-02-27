import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

object v {
  val scala = "3.1.1"
  val utest = ivy"com.lihaoyi::utest:0.7.11"
  val oslib = ivy"com.lihaoyi::os-lib:0.8.1"
}

object scirt extends ScalaModule with ScalafmtModule {
  def scalaVersion = v.scala

  override def ivyDeps = Agg(
    v.oslib
  )

  override def forkEnv: T[Map[String, String]] = T {
    val s = super.forkEnv()
    val m = collection.mutable.Map(s.toSeq: _*)
    m.update("PATH", s"${}:" + s.getOrElse("PATH", ""))
    m.toMap
  }

  object tests extends Tests with TestModule.Utest with ScalafmtModule {
    override def ivyDeps = Agg(
      v.utest
    )
  }
}
