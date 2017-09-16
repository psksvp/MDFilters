package psksvp

/**
  * created by psksvp@gmail.com Aug 15, 2017
  */
object MainEntryPoint
{
  def apply(filename:String):Unit=
  {
    import psksvp.Utils._
    import sys.process._
    val result = MDFilters.apply(readString(fromFileAtPath=filename))
    println(result)
  }
  
  def main(args:Array[String]):Unit=
  {
    if(args.length > 0)
      apply(args(0))
    else
      sys.error(psksvp.Utils.reportError("no md filename provided"))
  }
}