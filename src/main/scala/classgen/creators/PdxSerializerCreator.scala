package classgen.creators

import classgen.{Field, Util}

class PdxSerializerCreator(name: String, vars: List[Field], builder: Boolean)
  extends Creator {

  def fname: String = s"PdxSerializer$name.java"

  def writeStatements: String =
    Util.indent(
      vars
        .map(f => {
          val getter = s"x.get${f.name.capitalize}()"

          f.dataType.toLowerCase match {
            case "byte" => s"pos.writeByte($getter);"
            case "short" => s"pos.writeShort($getter);"
            case "int" => s"pos.writeInt($getter);"
            case "long" => s"pos.writeLong($getter);"
            case "float" => s"pos.writeFloat($getter);"
            case "double" => s"pos.writeDouble($getter);"
            case "string" => s"pos.writeString($getter);"
            case "biginteger" => s"pos.writeByteArray($getter.toByteArray());"
            case "bigdecimal" =>
              s"""pos.writeByteArray($getter.unscaledValue().toByteArray());
                 |pos.writeInt($getter.scale());
               """.stripMargin
            case "gregoriancalendar" => s"pos.writeLong($getter.getTimeInMillis());"
            case _ => s"pos.writeObject($getter, false);"
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
            case "byte" => s"$prefix = pis.readByte();"
            case "short" => s"$prefix = pis.readShort();"
            case "int" => s"$prefix = pis.readInt();"
            case "long" => s"$prefix = pis.readLong();"
            case "float" => s"$prefix = pis.readFloat();"
            case "double" => s"$prefix = pis.readDouble();"
            case "string" => s"$prefix = pis.readString();"
            case "biginteger" => s"$prefix = new BigInteger(pis.readByteArray());"
            case "bigdecimal" =>
              s"$prefix = new BigDecimal(new BigInteger(pis.readByteArray()), pis.readInt());"
            case "gregoriancalendar" =>
              s"""$prefix = (GregorianCalendar)Calendar.getInstance();
                 |${f.name}.setTimeInMillis(pos.readLong());
               """.stripMargin
            case _ => s"$prefix = pis.readObject();"
          }
        })
        .mkString("\n"),
      8
    )

  def create: String = {
    s"""public class PdxSerializer$name extends AbstractSerializer {
       |
       |    public byte[] serialize(Object object) {
       |        $name x = ($name)object);
       |
       |        PdxOutputStream pos = new PdxOutputStream();
       |
       |        $writeStatements
       |
       |        return pos.toByteArray();
       |    }
       |
       |
       |    public Object deserialize(byte[] ba, Class cls) {
       |        PdxInputStream pis = new PdxInputStream(ba);
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
