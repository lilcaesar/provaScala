import scala.collection.mutable
import scala.io._

object ShiftCipherDecrypter{
  def main(args: Array[String]){
    var cipherTextMessagePath: String = args(0)
    var charactersPath: String = args(1)

    var cipherTextMessage = Source.fromFile(cipherTextMessagePath).getLines.mkString

    //Calcolo le occorrenze del cypherText

    var characterCountsApp = new mutable.HashMap[Char, Int]

    //Inizializzo i valori a 0
    for(c <- 65 to 90){
      characterCountsApp.put(c.toChar, 0)
    }

    var i=0
    while (i < cipherTextMessage.length){
      var c= cipherTextMessage.charAt(i)
      if(characterCountsApp.contains(c))
        characterCountsApp.put(c, characterCountsApp(c)+1)
      else
        characterCountsApp.put(c,1)
      i+=1
    }
    var characterCounts = characterCountsApp.toSeq.sortBy(_._1)

    //Carico la mappa di occorrenze del dizionario
    var charactersOccurenciesMap = new mutable.HashMap[Char, Int]
    for(line <- Source.fromFile(charactersPath).getLines) {
      var values: Array[String] = line.split(" ")
      charactersOccurenciesMap.put(values(0).charAt(0),values(1).toInt)
    }
    var charactersOccurencies = charactersOccurenciesMap.toSeq.sortBy(_._1)

    //Calcolo Indice Coincidenza del dizionario dei caratteri
    var FIchar = 0L
    var dictionaryLength= 0L
    for(element <- charactersOccurencies){
      FIchar += (element._2.toLong * (element._2.toLong -1))
      dictionaryLength += element._2.toLong
    }
    var ICchar = FIchar.toDouble/(dictionaryLength*(dictionaryLength-1)).toDouble

    //Calcolo dei vari indici di mutua coincidenza
    var key = 0

    var IMC = 0L
    var IMFKey = 0D
    var k = 0
    while(k < 26) {
      IMC = 0L
      i = 0
      while(i < 26) {
        var fi=charactersOccurencies(i)._2
        var fi2=characterCounts((i+k)%26)._2
        IMC += fi.toLong * fi2.toLong
        i+=1
      }
      var newIMC= IMC.toDouble/(dictionaryLength * cipherTextMessage.length.toLong).toDouble
      if(Math.abs(newIMC-ICchar) < Math.abs(IMFKey-ICchar)) {
        IMFKey = newIMC
        key = k
      }
      k+=1
    }

    var decryption = cipherTextMessage.map(c => ((((c.toInt-65)+(26-key))%26)+65).toChar)
    println(s"Key is: $key\nPlain text is:\n")
    println(decryption)
  }
}
