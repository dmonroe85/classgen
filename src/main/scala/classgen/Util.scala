package classgen

import scala.io.Source

object Util {

  def returnAndIndent(sep: String, indent: Int): String =
    sep + (" " * indent)


  def indent(string: String, indentation: Int): String =
    string.replace("\n", "\n" + " " * indentation)

  def rebuildClass(name: String, vars: List[Field], builder: Boolean = false): String = {
    if (builder) {
      val setFields =
        vars
          .map(f => s"object.set${f.name.capitalize}(${f.name});")
          .mkString("\n")

      Util.indent(
        s"""$name object = new $name();
           |$setFields
         """.stripMargin,
        8)
    }
    else {
      s"$name object = new $name(${vars.map(_.name).mkString(",")});"
    }
  }

  def fieldsFromCSV(fname: String): List[Field] = {
    Source.fromFile(fname).getLines().map(line => {
      val Array(name, dataType) = line.split(",")
      Field(name, dataType)
    }).toList
  }


}
