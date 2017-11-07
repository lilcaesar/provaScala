import java.io.{File, PrintWriter}
import scala.io._

object PlainTextCreator {
  def main(args: Array[String]) {
    var textPath: String = args(0) //Dictionary
    var Text = Source.fromFile(textPath).getLines.mkString
    var plainFile = new PrintWriter(new File("PlainTextMessage.txt"))

    Text = Text.replace("è", "e")
    Text = Text.replace("é", "e")
    Text = Text.replace("ì", "i")
    Text = Text.replace("ò", "o")
    Text = Text.replace("ù", "u")
    Text = Text.replace("à", "a")
    Text = Text.replace("È", "E")
    Text = Text.replaceAll("\\s+", "")
    Text = Text.replaceAll("\\W+", "")
    Text = Text.replaceAll("([0-9])", "")
    Text = Text.replaceAll("([A-Z])", "$1").toLowerCase

    plainFile.write(Text)
    plainFile.close()
  }
}