package classgen.creators

import classgen.{Field, Util}

class FlyPackedClassCreator(name: String, vars: List[Field]) extends Creator{

  def fname: String = s"${name}Packed.java"

  def createPackedClassList: String =
    vars
      .map(f => s"${f.dataType}.class")
      .mkString(Util.returnAndIndent(",\n", 12))


  def createPackedClassBufferList: String =
    vars
      .map(f => s"dos.write(serialize(x.get${f.name.capitalize}()));")
      .mkString(Util.returnAndIndent("\n", 8))


  def createPackedClassGetList: String =
    vars.zipWithIndex.map{ case (f: Field, i: Int) => {
      s"public ${f.dataType} get${f.name.capitalize}() throws IOException, ClassNotFoundException" +
        s" { return (${f.dataType})dataFromPayload($i); }"
    }}.mkString(Util.returnAndIndent("\n", 4))


  def createPackedClassEquals: String = {
    val equalAnds =
      vars
        .map(f => {
          val getName = s"get${f.name.capitalize}"
          s"$getName().equals(that.$getName())"
        })
        .mkString(Util.returnAndIndent(" &\n", 8))

    val exceptions =
      s"""catch (IOException e) {
         |    return false;
         |}
         |catch (ClassNotFoundException e) {
         |    return false;
         |}
       """.stripMargin

    Util.indent(
      s"""try {
         |    return (
         |        $equalAnds
         |    );
         |}
         |$exceptions
     """.stripMargin,
      12
    )
  }


  def create: String = {
    val packedName = s"${name}Packed"


    s"""public class $packedName extends FlyPackedClass {
       |
       |    @Override
       |    public void setClasses() {
       |        classes = new Class[]{
       |            $createPackedClassList
       |        };
       |    }
       |
       |    public $packedName($name x) throws IOException {
       |        super();
       |
       |        ByteArrayOutputStream baos = new ByteArrayOutputStream();
       |        DataOutputStream dos = new DataOutputStream(baos);
       |
       |        $createPackedClassBufferList
       |
       |        int position = buffer.position();
       |        setPayload(baos.toByteArray());
       |
       |        dos.close();
       |    }
       |
       |    $createPackedClassGetList
       |
       |    @Override
       |    public int hashCode() { return super.hashCode(); }
       |
       |    @Override
       |    public boolean equals(Object o) {
       |        if (o == this) { return true; }
       |        else if (o.getClass() == $name.class) {
       |            $name that = ($name)o;
       |
       |            $createPackedClassEquals
       |        }
       |        else if (o.getClass() == $packedName.class) {
       |            $packedName that = ($packedName)o;
       |
       |            $createPackedClassEquals
       |        }
       |
       |        return false;
       |    }
       |}
     """.stripMargin
  }

}
