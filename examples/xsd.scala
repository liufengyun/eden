import scala.annotation.StaticAnnotation
import scala.meta._
import scala.meta.dialects.Dotty

import scala.collection.immutable.Seq

class xsd(fileName: String) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"object $name" =>
        val a = 3
        q"val x = $a" // test lifting inside meta block

        val q"new xsd(${fileName: String})" = this
        val schema = loadSchema(fileName).map(xsdComplexType => {
          val name = Type.Name(xsdComplexType.name)
          val fields = xsdComplexType.fields.map(xsdField => {
            val fieldName = Term.Name(xsdField.name)
            val fieldType = xsdField.tpe.parse[Type].get
            param"$fieldName: ${Some(fieldType)}"
          })
          q"case class $name(..$fields)"
        })
        q"object $name { ..$schema }"
      case _ =>
        abort("@xsd can only annotate objects")
    }
  }
}

case class XsdComplexType(name: String, fields: List[XsdField])
case class XsdField(name: String, tpe: String)

object loadSchema {
  def apply(fileName: String): List[XsdComplexType] = {
    val xmlSchema = scala.xml.XML.load(new java.io.FileInputStream(fileName))
    (xmlSchema \ "element").toList.map(xmlElement => {
      val schemaName = xmlElement \@ "name"
      val xmlFields = xmlElement \ "complexType" \ "sequence" \ "element"
      val schemaFields = xmlFields.toList.map(xmlField => {
        val schemaName = xmlField \@ "name"
        val schemaType = (xmlField \@ "type") match {
          case "xs:string" => "_root_.java.lang.String"
          case other => sys.error("unsupported field type: " + other)
        }
        XsdField(schemaName, schemaType)
      })
      XsdComplexType(schemaName, schemaFields)
    })
  }
}
