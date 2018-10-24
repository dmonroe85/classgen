package classgen.creators

import classgen.{Field, Util}

class AvroSerializerCreator(name: String, vars: List[Field], builder: Boolean = false)
  extends Creator {

  def fname: String = s"AvroSerializer$name.java"

  def putStatements: String =
    vars
      .map(f => {
        val getter =
          f.dataType.toLowerCase() match {
            case "biginteger" => s"ByteBuffer.wrap(x.get${f.name.capitalize}().toByteArray())"
            case "bigdecimal" => s"ByteBuffer.wrap(Bytes.toBytes(x.get${f.name.capitalize}()))"
            case "gregoriancalendar" => s"x.get${f.name.capitalize}().getTimeInMillis()"
            case _ => s"""x.get${f.name.capitalize}()"""
          }

        s"""payload.put("${f.name}", $getter);"""
      })
      .mkString(Util.returnAndIndent("\n", 8))

  def typeSpecficGet(f: Field): String =
    Util.indent(
      f.dataType.toLowerCase() match {
        case "byte" => s"""return ((Integer)payload.get("${f.name}")).byteValue();"""
        case "short" => s"""return ((Integer)payload.get("${f.name}")).shortValue();"""
        case "int" => s"""return ((Integer)payload.get("${f.name}"));"""
        case "long" => s"""return ((Long)payload.get("${f.name}"));"""
        case "float" => s"""return ((Float)payload.get("${f.name}"));"""
        case "double" => s"""return ((Double)payload.get("${f.name}"));"""
        case "biginteger" => s"""return new BigInteger(((ByteBuffer)payload.get("${f.name}")).array());"""
        case "bigdecimal" => s"""return Bytes.toBigDecimal(((ByteBuffer)payload.get("${f.name}")).array());"""
        case "string" => s"""return ((Utf8)payload.get("${f.name}")).toString();"""
        case "gregoriancalendar" =>
          s"""GregorianCalendar gc = (GregorianCalendar) Calendar.getInstance();
             |get.setTimeInMillis((Long)payload.get("${f.name}"));
             |return gc;
           """.stripMargin
      },
      4
    )


  def getFunctions: String = {
    Util.indent(
      vars
        .map(f =>
          s"""private ${f.dataType} get${f.name.capitalize}(GenericRecord payload) {
             |    ${typeSpecficGet(f)}
             |}
             |
             |public ${f.dataType} get${f.name.capitalize}(byte[] ba) throws IOException {
             |    return get${f.name.capitalize}(parseGenericRecord(ba));
             |}
             |
           """.stripMargin)
        .mkString("\n"),
      4
    )
  }

  def readInputs: String =
    vars
      .map(f => s"${f.dataType} ${f.name} = get${f.name.capitalize}(payload);")
      .mkString(Util.returnAndIndent("\n", 8))



  def create: String =
    s"""public class AvroSerializer$name extends AbstractSerializer {
       |    private Schema schema = AvroSchemas.${name.toLowerCase}Schema();
       |
       |    public byte[] serialize(Object object) throws IOException {
       |        $name x = ($name)object;
       |
       |        GenericRecord payload = new GenericData.Record(schema);
       |
       |        $putStatements
       |
       |        ByteArrayOutputStream baos = new ByteArrayOutputStream();
       |        BinaryEncoder encoder = EncoderFactory.get().binaryEncoder(baos, null);
       |        DatumWriter<GenericRecord> writer = new GenericDatumWriter<GenericRecord>(schema);
       |
       |        writer.write(payload, encoder);
       |        encoder.flush();
       |        baos.close();
       |
       |        return baos.toByteArray();
       |    }
       |
       |
       |    private GenericRecord parseGenericRecord(byte[] ba) throws IOException {
       |        GenericRecord payload = new GenericData.Record(schema);
       |
       |        ByteArrayInputStream bais = new ByteArrayInputStream(ba);
       |        BinaryDecoder decoder = DecoderFactory.get().binaryDecoder(bais, null);
       |
       |        DatumReader<GenericRecord> reader = new GenericDatumReader<GenericRecord>(schema);
       |        reader.read(payload, decoder);
       |
       |        bais.close();
       |
       |        return payload;
       |    }
       |
       |
       |    $getFunctions
       |
       |    public Object deserialize(byte[] ba, Class cls) throws IOException {
       |        GenericRecord payload = parseGenericRecord(ba);
       |
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
