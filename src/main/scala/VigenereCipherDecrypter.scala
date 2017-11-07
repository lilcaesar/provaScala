import scala.collection.mutable
import scala.io._

object VigenereCipherDecrypter{

  def computeLengthByOccurrencies(occurrencies: Seq[(Char, Int)]): Int ={
    var length=0
    for(element <- occurrencies){
      length += element._2
    }
    return length
  }

  def computeOccurrencies(text: String): Seq[(Char, Int)] ={
    var occurrenciesMap = new mutable.HashMap[Char, Int]

    //Inizializzo a 0
    for(c <- 65 to 90){
      occurrenciesMap.put(c.toChar, 0)
    }

    var i=0
    while (i < text.length){
      var c= text.charAt(i)
      if(occurrenciesMap.contains(c))
        occurrenciesMap.put(c, occurrenciesMap(c)+1)
      else
        occurrenciesMap.put(c,1)
      i+=1
    }
    var dictionaryOccurencies = occurrenciesMap.toSeq.sortBy(_._1)

    return dictionaryOccurencies
  }

  def computeIC(occurrencies: Seq[(Char, Int)]) : Double = {
    var FIchar = 0L
    var dictionaryLength= 0L
    for(element <- occurrencies){
      FIchar += (element._2.toLong * (element._2.toLong -1))
      dictionaryLength += element._2.toLong
    }
    var ICchar = FIchar.toDouble/(dictionaryLength*(dictionaryLength-1)).toDouble
    return ICchar
  }

  def computeShiftDecrypter(dictionaryOccurencies: Seq[(Char, Int)], cipherTextOccurrencies: Seq[(Char, Int)],
                            dictionaryLength: Int, cipherTextLength: Int, dictionaryIC: Double): Int ={
    var IMC = 0L
    var IMFKey = 0D
    var key = 0
    var k = 0
    while(k < 26) {
      IMC = 0L
      var i = 0
      while(i < 26) {
        var fi=dictionaryOccurencies(i)._2
        var fi2=cipherTextOccurrencies((i+k)%26)._2
        IMC += fi.toLong * fi2.toLong
        i+=1
      }
      var newIMC= IMC.toDouble/(dictionaryLength * cipherTextLength.toLong).toDouble
      if(Math.abs(newIMC-dictionaryIC) < Math.abs(IMFKey-dictionaryIC)) {
        IMFKey = newIMC
        key = k
      }
      k+=1
    }
    return key
  }

  def main(args: Array[String]){
    var cipherTextMessagePath: String = args(0)
    var charactersPath: String = args(1)

    var cipherTextMessage = Source.fromFile(cipherTextMessagePath).getLines.mkString

    //Carico la mappa di occorrenze del dizionario
    var dictionaryOccurenciesMap = new mutable.HashMap[Char, Int]
    for(line <- Source.fromFile(charactersPath).getLines) {
      var values: Array[String] = line.split(" ")
      dictionaryOccurenciesMap.put(values(0).charAt(0),values(1).toInt)
    }
    var dictionaryOccurencies = dictionaryOccurenciesMap.toSeq.sortBy(_._1)

    //Calcolo Indice Coincidenza del dizionario dei caratteri
    var dictionaryIC = computeIC(dictionaryOccurencies)

    var m=1
    var key=""

    var temporaryM = 1
    while(temporaryM < cipherTextMessage.length){
      if(cipherTextMessage.length%temporaryM==0){
        var temporaryCipherText = ""
        cipherTextMessage.grouped(temporaryM).toList.foreach(temporaryCipherText+= _.charAt(0).toString)
        var temporaryCipherTextOccurrencies = computeOccurrencies(temporaryCipherText)
        var temporaryCipherTextIC = computeIC(temporaryCipherTextOccurrencies)
        if(Math.abs(temporaryCipherTextIC - dictionaryIC) < 0.001){
          m=temporaryM
          temporaryM = cipherTextMessage.length
        }
      }
      temporaryM +=1
    }
    //Ora che abbiamo trovato la m computiamo m volte l'algoritmo ShiftCipher per decrittare il messaggio e trovare la chiave
    var dictionaryLength = computeLengthByOccurrencies(dictionaryOccurencies)
    for(i <- 0 until m){
      var temporaryCipherText = ""
      cipherTextMessage.grouped(m).toList.foreach(temporaryCipherText+= _.charAt(i).toString)
      var temporaryCipherTextOccurrencies = computeOccurrencies(temporaryCipherText)
      var temporaryCipherTextLength = computeLengthByOccurrencies(temporaryCipherTextOccurrencies)
      key += (computeShiftDecrypter(dictionaryOccurencies, temporaryCipherTextOccurrencies, dictionaryLength, temporaryCipherTextLength, dictionaryIC)+65).toChar.toString
    }
    var decryption = ""
    for(i <- 0 until cipherTextMessage.length by key.length){
      for(k <- 0 until key.length){
        decryption += s"${((((cipherTextMessage.charAt(i+k).toInt-65)+26-(key.charAt(k).toInt-65))%26)+65).toChar}"
      }
    }
    println(s"Chiave: $key")
    println(s"Testo decifrato:\n $decryption")
  }
}
