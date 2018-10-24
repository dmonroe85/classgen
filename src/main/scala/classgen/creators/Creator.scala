package classgen.creators

import java.io.{File, PrintWriter}

trait Creator {

  def fname: String

  def create: String

  def writeFile: Unit = {
    val f = new File(fname)
    val p = new PrintWriter(f)

    try {
      p.println(create)
    } finally {
      p.close()
    }
  }

}
