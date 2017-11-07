import java.io.{File, PrintWriter}
import scala.io._

object ShiftCipherEncrypter{
  def main(args: Array[String]){
    var plainTextMessagePath: String = args(0)
    var key: Int = args(1).toInt
    var cryptedMessage = new PrintWriter(new File("ShiftEncryption.txt"))

    var plainTextMessage = Source.fromFile(plainTextMessagePath).getLines.mkString

    var encryption = plainTextMessage.map(c => ((((c.toInt-97)+key)%26)+97).toChar)
    cryptedMessage.write(encryption)
    cryptedMessage.close()
  }
}
