package classgen.creators

import classgen.{Field, Util}

class KryoSerializerCreator(name: String, vars: List[Field], builder: Boolean = false)
  extends Creator {

  def fname: String = s"${name}KryoSerializer.java"


  def writeOutputs: String = {
    vars.map(f => {
      val objectCall = s"object.get${f.name.capitalize}()"

      f.dataType.toLowerCase match {
        case "byte" => s"output.writeByte($objectCall);"
        case "short" => s"output.writeShort($objectCall);"
        case "int" => s"output.writeInt($objectCall);"
        case "long" => s"output.writeLong($objectCall);"
        case "float" => s"output.writeFloat($objectCall);"
        case "double" => s"output.writeDouble($objectCall);"
        case "string" => s"output.writeString($objectCall);"
        case "gregoriancalendar" => s"output.writeLong($objectCall.getTimeInMillis());"
        case _ => s"kryo.writeObject(output, $objectCall);"
      }
    }).mkString(Util.returnAndIndent("\n", 8))
  }

  def readInputs: String = {
    vars.map(f => {
      val prefix = s"${f.dataType} ${f.name}"

      f.dataType.toLowerCase match {
        case "byte" => s"$prefix = input.readByte();"
        case "short" => s"$prefix = input.readShort();"
        case "int" => s"$prefix = input.readInt();"
        case "long" => s"$prefix = input.readLong();"
        case "float" => s"$prefix = input.readFloat();"
        case "double" => s"$prefix = input.readDouble();"
        case "string" => s"$prefix = input.readString();"
        case "gregoriancalendar" =>
          Util.indent(
          s"""$prefix = (GregorianCalendar) Calendar.getInstance();
             |${f.name}.setTimeInMillis(input.readLong);
           """.stripMargin,
            8
          )
        case _ => s"$prefix = kryo.readObject(input, ${f.dataType}.class);"
      }
    }).mkString(Util.returnAndIndent("\n", 8))
  }

  def create: String = {
    s"""public class ${name}KryoSerializer extends Serializer<$name> {
       |
       |    @Override
       |    public void write(Kryo kryo, Output output, $name object) {
       |        $writeOutputs
       |    }
       |
       |    @Override
       |    public $name read(Kryo kryo, Input input, Class<? extends $name> type) {
       |        $readInputs
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
