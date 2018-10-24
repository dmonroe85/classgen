package classgen

import classgen.creators._


object App extends App {
  val fname = "./src/main/resources/Simple.csv"
  val className = fname.split("""\/""").last.split("""\.""").head
  val vars = Util.fieldsFromCSV(fname)

  val creator = new PdxSerializerCreator(className, vars, false)

  creator.writeFile
}
