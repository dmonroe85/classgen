package classgen.creators

import classgen.{Field, Util}

class DataSerializerCreator(name: String, vars: List[Field], builder: Boolean)
  extends Creator {

  def fname: String = s"DataSerializer$name.java"

  def writeStatements: String =
    Util.indent(
      vars
        .map(f => {
          val getter = s"x.get${f.name.capitalize}()"

          f.dataType.toLowerCase match {
            case "byte" => s"dos.writeByte($getter);"
            case "short" => s"dos.writeShort($getter);"
            case "int" => s"dos.writeInt($getter);"
            case "long" => s"dos.writeLong($getter);"
            case "float" => s"dos.writeFloat($getter);"
            case "double" => s"dos.writeDouble($getter);"
            case "string" => s"dos.writeUTF($getter);"
            case "biginteger" => s"DataSerializer.writeByteArray($getter.toByteArray(), dos);"
            case "bigdecimal" =>
              s"""DataSerializer.writeByteArray($getter.unscaledValue().toByteArray(), dos);
                 |dos.writeInt($getter.scale());
               """.stripMargin
            case "gregoriancalendar" => s"dos.writeLong($getter.getTimeInMillis());"
            case _ => s"DataSerializer.writeObject($getter, dos);"
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
            case "byte" => s"$prefix = dis.readByte();"
            case "short" => s"$prefix = dis.readShort();"
            case "int" => s"$prefix = dis.readInt();"
            case "long" => s"$prefix = dis.readLong();"
            case "float" => s"$prefix = dis.readFloat();"
            case "double" => s"$prefix = dis.readDouble();"
            case "string" => s"$prefix = dis.readUTF();"
            case "biginteger" => s"$prefix = new BigInteger(DataSerializer.readByteArray(dis));"
            case "bigdecimal" =>
              s"$prefix = new BigDecimal(new BigInteger(DataSerializer.readByteArray(dis)), dis.readInt());"
            case "gregoriancalendar" =>
              s"""$prefix = (GregorianCalendar) Calendar.getInstance();
                 |${f.name}.setTimeInMillis(dis.readLong());
               """.stripMargin
            case _ => s"$prefix = DataSerializer.readObject(dis);"
          }
        })
        .mkString("\n"),
      8
    )

  def create: String = {
    s"""public class DataSerializer$name extends AbstractSerializer {
       |
       |    public byte[] serialize(Object object) throws IOException {
       |        $name x = ($name)object;
       |
       |        ByteArrayOutputStream baos = new ByteArrayOutputStream();
       |        DataOutputStream dos = new DataOutputStream(baos);
       |
       |        $writeStatements
       |
       |        dos.flush();
       |        dos.close();
       |
       |        return baos.toByteArray();
       |    }
       |
       |
       |    public Object deserialize(byte[] ba, Class cls) throws IOException {
       |        ByteArrayInputStream bais = new ByteArrayInputStream(ba);
       |        DataInputStream dis = new DataInputStream(bais);
       |
       |        $readStatements
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
