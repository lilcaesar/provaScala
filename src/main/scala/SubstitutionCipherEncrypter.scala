import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.io._

object SubstitutionCipherEncrypter{
  def main(args: Array[String]){
    var plainTextMessagePath: String = args(0)
    var key: String = args(1)
    var cryptedMessage = new PrintWriter(new File("SubstitutionEncryption.txt"))

    var plainTextMessage = Source.fromFile(plainTextMessagePath).getLines.mkString

    var keyMap= new mutable.HashMap[Char, Char]
    var intOfCharacter = 97
    for(c <- key){
      keyMap.put(c, intOfCharacter.toChar)
      intOfCharacter+=1
    }

    println(keyMap)

    var encryption = plainTextMessage.map(c => keyMap(c))
    cryptedMessage.write(encryption)
    cryptedMessage.close()
  }
}
