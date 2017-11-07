import java.io.{File, PrintWriter}
import scala.io._

object PermutationCipherEncrypter{
  def main(args: Array[String]){
    var plainTextMessagePath: String = args(0)
    var key: String = args(1)
    var cryptedMessage = new PrintWriter(new File("PermutationEncryption.txt"))

    var plainTextMessage = Source.fromFile(plainTextMessagePath).getLines.mkString
    var m = key.length
    var textLength = plainTextMessage.length
    var paddingLength = m-textLength%m

    for(i <- 0 until paddingLength){
      plainTextMessage += "X"
    }

    textLength = plainTextMessage.length
    var encryption = ""
    for(i <- 0 until textLength by m){
      for(j <- 0 until m){
        encryption += plainTextMessage.charAt(i+(key.charAt(j).toString.toInt))
      }
    }

    cryptedMessage.write(encryption)
    cryptedMessage.close()
  }
}