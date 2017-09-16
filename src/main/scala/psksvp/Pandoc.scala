package psksvp

/**
  * created by psksvp@gmail.com Aug 15, 2017
  */
object Pandoc
{
  def apply(filename:String):Unit=
  {
    //pandoc -F pandoc-crossref -o "$basename.pdf"
    import psksvp.MDFilters._
    import psksvp.Utils._
    import sys.process._
    val result = MDFilters.apply(readString(fromFileAtPath=filename))
    //println(result)
    val tempFile = toFile(result, ".md")
    val output = Seq("pandoc", "--number-sections", "-F", "pandoc-crossref", "-o", s"$filename.pdf", tempFile).!!
    if(output.trim.nonEmpty)
    {
      sys.error(reportError(s"pandoc error: $output"))
    }
  }
  
  def main(args:Array[String]):Unit=
  {
    if(args.length > 0)
      apply(args(0))
    else
      sys.error(psksvp.Utils.reportError("no md filename provided"))
  }
}