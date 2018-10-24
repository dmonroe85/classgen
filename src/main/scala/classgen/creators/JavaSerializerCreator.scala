package classgen.creators

import classgen.{Field, Util}

class JavaSerializerCreator(name: String, vars: List[Field], builder: Boolean)
  extends Creator {

  def fname: String = s"JavaSerializer$name.java"

  def writeStatements: String =
    Util.indent(
      vars
        .map(f => {
          val getter = s"x.get${f.name.capitalize}()"

          f.dataType.toLowerCase match {
            case "byte" => s"oos.writeByte($getter);"
            case "short" => s"oos.writeShort($getter);"
            case "int" => s"oos.writeInt($getter);"
            case "long" => s"oos.writeLong($getter);"
            case "float" => s"oos.writeFloat($getter);"
            case "double" => s"oos.writeDouble($getter);"
            case "string" => s"oos.writeUTF($getter);"
            case "gregoriancalendar" => s"oos.writeLong($getter.getTimeInMillis());"
            case _ => s"oos.writeObject($getter);"
          }
        })
        .mkString("\n"),
      8
    )

  def readStatements: String =
    Util.indent(
      vars
        .map(f => {
          val prefix = s"${f.dataType} ${f.name}"

          f.dataType.toLowerCase match {
            case "byte" => s"$prefix = ois.readByte();"
            case "short" => s"$prefix = ois.readShort();"
            case "int" => s"$prefix = ois.readInt();"
            case "long" => s"$prefix = ois.readLong();"
            case "float" => s"$prefix = ois.readFloat();"
            case "double" => s"$prefix = ois.readDouble();"
            case "string" => s"$prefix = ois.readUTF();"
            case "gregoriancalendar" =>
              s"""$prefix = (GregorianCalendar) Calendar.getInstance();
                 |${f.name}.setTimeInMillis(ois.readLong());
               """.stripMargin
            case _ => s"$prefix = (${f.dataType})ois.readObject();"
          }
        })
        .mkString("\n"),
      8
    )

  def create: String = {
    s"""public class JavaSerializer$name extends AbstractSerializer {
       |
       |    public byte[] serialize(Object object) throws IOException {
       |        $name x = ($name)object;
       |        ByteArrayOutputStream baos = new ByteArrayOutputStream();
       |
       |        ObjectOutputStream oos = new ObjectOutputStream(baos);
       |
       |        $writeStatements
       |
       |        oos.close();
       |
       |        return baos.toByteArray();
       |    }
       |
       |
       |    public Object deserialize(byte[] ba, Class cls) throws IOException, ClassNotFoundException {
       |        ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(ba));
       |
       |        $readStatements
       |
       |        ois.close();
       |
       |        ${Util.rebuildClass(name, vars, builder)}
       |
       |        return object;
       |    }
       |
       |}
     """.stripMargin
  }

}
