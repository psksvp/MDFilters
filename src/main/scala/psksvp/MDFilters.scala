package psksvp

/**
  * created by psksvp@gmail.com Aug 15, 2017
  */
object MDFilters
{ 
  import psksvp.Utils._
   
  def apply(text:String):String=
  {
    makeDirectory("./images")
    val a = filterGraphviz(text, "pdf", outputPath = "./images")
    filterCSV(a)
  }

  def filterCSV(mdSource:String):String=
  {
    case class CSVTable(attribute:String, csvSource:String)

    def patternMatch(text:String):Set[(String, CSVTable)]=
    {
      val csvtable = """(?s)~~~\s*csvtable\s*\{(.*?)\}(.*?)~~~""".r
      val r = for (vfes <- csvtable.findAllIn(mdSource)) yield
              {
                vfes match
                {
                  case csvtable(attrib, csvSrc) => Set((vfes, CSVTable(attrib, csvSrc.trim)))
                  case _ => Set[(String, CSVTable)]()
                }
              }

      r.reduceLeft(_ union _)
    }

    def transform(g:CSVTable):String=
    {
      import sys.process._

      val caption = """caption\s*=\s*\"(.*?)\"""".r

      val captionText = caption.findFirstIn(g.attribute).getOrElse("") match
      {
        case caption(text) => text
        case _             => "No caption"
      }

      val csvFile = toFile(g.csvSource, ".csv")
      //val tableText = Seq("csvtomd", csvFile).!!
      val tableText = Seq("csv2md", "--pretty", csvFile).!!
      val result = new StringBuilder()
      result.append(s": $captionText {${g.attribute.split(" ").head}}\n\n")
      result.append(tableText)
      result.append("\n")
      result.toString
    }

    def runFilter(s:Set[(String, CSVTable)]):Set[(String, String)]=
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


  /**
    *
    * @param mdSource
    * @param outputType
    * @param outputPath
    * @return
    */
  def filterGraphviz(mdSource:String,
                     outputType:String = "pdf",
                     outputPath:String = "."):String=
  {
    case class GraphViz(attribute:String, dot:String)

    def patternMatch(text:String):Set[(String,GraphViz)]=
    {
      /*
        (?s)   keep looking pass new line
        \s*    skip spaces
        (.*?)  a group of wild card which can be sucked up
        \{     esc for char {
        \}     esc for char }
       */
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
        case _             => sys.error(reportError("cound not lift graph name from dot. Graph name is missing? eg digraph nameHere { }"))
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
