package psksvp

/**
  * created by psksvp@gmail.com Aug 15, 2017
  */
object MDFilters
{ 
  import psksvp.Utils._
   
  def apply(text:String):String=
  {
    filterGraphviz(text)
  }
  
  def filterGraphviz(mdSource:String,
                     outputType:String = "pdf",
                     outputPath:String = "."):String=
  {
    case class GraphViz(attribute:String, dot:String)

    def patternMatch(text:String):Set[(String,GraphViz)]=
    {
      val graphviz = """(?s)~~~\s*graphviz\s*\{(.*?)\}(.*?)~~~""".r
      val r = for (vfes <- graphviz.findAllIn(text)) yield
              {
                vfes match
                {
                  case graphviz(attrib, dot) => Set((vfes, GraphViz(attrib, dot.trim)))
                  case _                     => Set[(String, GraphViz)]()
                }
              }

      r.reduceLeft(_ union _)
    }

    def dot(src:String):String=
    {
      val graphType = """(?s)\s*(digraph|graph)\s*(.*?)\s*\{""".r
      val digraph = """(?s)\s*digraph\s*(.*?)\s*\{""".r
      val graph = """(?s)\s*graph\s*(.*?)\s*\{""".r

      val graphname = graphType.findFirstIn(src).getOrElse("") match
      {
        case digraph(name) => name
        case graph(name)   => name
        case _             => sys.error("cound not match for graph name")
      }

      val filepath = s"$outputPath/$graphname.$outputType"

      import sys.process._
      val error = Seq("dot", s"-T$outputType", s"-o$filepath", toFile(src, ".dot")).!!
      if(error.trim.nonEmpty)
        sys.error(reportError(s"dot error:\n$error"))
      else
        filepath
    }


    def transform(g:GraphViz):String=
    {
      val caption = """caption\s*=\s*\"(.*?)\"""".r

      val captionText = caption.findFirstIn(g.attribute).getOrElse("") match
      {
        case caption(text) => text
        case _             => "No caption"
      }

      val dotOutputPath = dot(g.dot)
      s"![$captionText]($dotOutputPath){${g.attribute}}"
    }

    def runFilter(s:Set[(String, GraphViz)]):Set[(String, String)]=
    {
      for(p <- s) yield
      {
        (p._1, transform(p._2))
      }
    }

    def go():String=
    {
      val p = runFilter(patternMatch(mdSource))
      searchAndReplace(p, mdSource)
    }

    go()
  }
}
