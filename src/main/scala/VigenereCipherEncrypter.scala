import java.io.{File, PrintWriter}
import scala.io._

object VigenereCipherEncrypter{
  def main(args: Array[String]){
    var plainTextMessagePath: String = args(0)
    var key: String = args(1)
    var cryptedMessage = new PrintWriter(new File("VigenereEncryption.txt"))

    var plainTextMessage = Source.fromFile(plainTextMessagePath).getLines.mkString

    //Aggiungo il padding
    for(i <- 0 until key.length-(plainTextMessage.length%key.length)){
      plainTextMessage+= "x"
    }

    var encryption = ""
    for(i <- 0 until plainTextMessage.length by key.length){
      for(k <- 0 until key.length){
        encryption += s"${((((plainTextMessage.charAt(i+k).toInt-97)+(key.charAt(k).toInt-97))%26)+97).toChar}"
      }
    }

    cryptedMessage.write(encryption)
    cryptedMessage.close()
  }
}

