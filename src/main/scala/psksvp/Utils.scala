package psksvp

/**
  * created by psksvp@gmail.com Aug 15, 2017
  */
object Utils
{
  def searchAndReplace(p:Set[(String, String)], s:String):String=
  {
    if(p.size > 0)
    {
      val pair = p.head
      val rs = s.replaceAllLiterally(pair._1, pair._2)
      searchAndReplace(p.tail, rs)
    }
    else
      s
  }

  def makeDirectory(path:String):Unit=
  {
    import sys.process._
    Seq("mkdir", "-p", path).!
  }

  def toFile(code:String, fileExt:String):String=
  {
    var tmpDir = System.getProperty("java.io.tmpdir")
    if(tmpDir.last != '/') tmpDir = tmpDir + "/"
    val file = scala.util.Random.alphanumeric.take(10).mkString
    val fileName = tmpDir + file + fileExt
    writeString(code, toFileAtPath = fileName)
    fileName
  }

  def readString(fromFileAtPath:String):String =
  {
    import sys.process._
    Seq("cat", fromFileAtPath).!!
  }

  def writeString(s:String, toFileAtPath:String):Unit=
  {
    import java.io.PrintWriter
    new PrintWriter(toFileAtPath)
    {
      write(s)
      close()
    }
  }

  def reportError(s:String):String = s"MDFilters ran into problems:\n$s"
}