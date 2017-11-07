import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.io._

object OccurrenciesExtractor {
  def main(args: Array[String]) {
    var dictionaryPath: String = args(0)  //Dictionary
    var dictionaryText = Source.fromFile(dictionaryPath).getLines.mkString
    var characters = new PrintWriter(new File("characters.txt"))
    var digrams = new PrintWriter(new File("digrams.txt"))
    var trigrams = new PrintWriter(new File("trigrams.txt"))

    dictionaryText= dictionaryText.replace("è", "e")
    dictionaryText= dictionaryText.replace("é", "e")
    dictionaryText= dictionaryText.replace("ì", "i")
    dictionaryText= dictionaryText.replace("ò", "o")
    dictionaryText= dictionaryText.replace("ù", "u")
    dictionaryText= dictionaryText.replace("à", "a")
    dictionaryText= dictionaryText.replace("È", "E")
    dictionaryText= dictionaryText.replaceAll("\\s+","")
    dictionaryText= dictionaryText.replaceAll("\\W+","")
    dictionaryText= dictionaryText.replaceAll("([0-9])","")
    dictionaryText= dictionaryText.replaceAll("([A-Z])", "$1").toUpperCase()

    //Conto le occorrenze delle lettere tramite una fold left
    var characterCountsApp = new mutable.HashMap[Char, Int]

    //Inizializzo i valori a 0
    for(c <- 65 to 90){
      characterCountsApp.put(c.toChar, 0)
    }

    var i=0
    while (i < dictionaryText.length){
      var c= dictionaryText.charAt(i)
      if(characterCountsApp.contains(c))
        characterCountsApp.put(c, characterCountsApp(c)+1)
      else
        characterCountsApp.put(c,1)
      i+=1
    }

    var characterCounts = characterCountsApp.toSeq.sortBy(_._1)

    for(element <- characterCounts){
      var line = s"${element._1} ${element._2}\n"
      characters.write(line)
    }
    characters.close()

    //Digrammi
    var digramCountsApp = new mutable.HashMap[String, Int]

    //Inizializzo i valori a 0
    for(c <- 65 to 90){
      for(c2 <- 65 to 90){
        digramCountsApp.put(s"${c.toChar}${c2.toChar}", 0)
      }
    }

    i=0
    while (i < (dictionaryText.length -1)){
      var c= s"${dictionaryText.charAt(i)}${dictionaryText.charAt(i+1)}"
      if(digramCountsApp.contains(c))
        digramCountsApp.put(c, digramCountsApp(c)+1)
      else
        digramCountsApp.put(c,1)
      i+=1
    }

    var digramCounts = digramCountsApp.toSeq.sortBy(_._1)

    for(element <- digramCounts){
      var line = s"${element._1} ${element._2}\n"
      digrams.write(line)
    }
    digrams.close()


    //Trigrammi
    var trigramCountsApp = new mutable.HashMap[String, Int]

    //Inizializzo i valori a 0
    for(c <- 65 to 90){
      for(c2 <- 65 to 90){
        for(c3 <- 65 to 90) {
          trigramCountsApp.put(s"${c.toChar}${c2.toChar}${c3.toChar}", 0)
        }
      }
    }

    i=0
    while (i < (dictionaryText.length -2)){
      var c= s"${dictionaryText.charAt(i)}${dictionaryText.charAt(i+1)}${dictionaryText.charAt(i+2)}"
      if(trigramCountsApp.contains(c))
        trigramCountsApp.put(c, trigramCountsApp(c)+1)
      else
        trigramCountsApp.put(c,1)
      i+=1
    }

    var trigramCounts = trigramCountsApp.toSeq.sortBy(_._1)

    for(element <- trigramCounts){
      var line = s"${element._1} ${element._2}\n"
      trigrams.write(line)
    }
    trigrams.close()
  }
}