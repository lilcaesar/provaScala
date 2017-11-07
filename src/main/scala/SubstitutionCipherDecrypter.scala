import scala.collection.mutable
import scala.io._

object SubstitutionCipherDecrypter{

  def computeCharOccurrencies(text: String): Seq[(Char, Int)] ={
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

  def computeDigramsOccurrencies(text: String): Seq[(String, Int)] ={
    var occurrenciesMap = new mutable.HashMap[String, Int]

    //Inizializzo a 0
    for(c <- 65 to 90){
      for(c2 <- 65 to 90) {
        occurrenciesMap.put(s"${c.toChar}${c2.toChar}", 0)
      }
    }

    var i=0
    while (i < text.length-1){
      var c= s"${text.charAt(i)}${text.charAt(i+1)}"
      if(occurrenciesMap.contains(c))
        occurrenciesMap.put(c, occurrenciesMap(c)+1)
      else
        occurrenciesMap.put(c,1)
      i+=1
    }
    var dictionaryOccurencies = occurrenciesMap.toSeq.sortBy(_._1)

    return dictionaryOccurencies
  }

  def computeLengthByOccurrencies(occurrencies: Seq[(Char, Int)]): Int ={
    var length=0
    for(element <- occurrencies){
      length += element._2
    }
    return length
  }

  def computeIC(occurrencies: Seq[(String, Int)]) : Double = {
    var FIchar = 0L
    var dictionaryLength= 0L
    for(element <- occurrencies){
      FIchar += (element._2.toLong * (element._2.toLong -1))
      dictionaryLength += element._2.toLong
    }
    var ICchar = FIchar.toDouble/(dictionaryLength*(dictionaryLength-1)).toDouble
    return ICchar
  }

  def computeDigramsMIC(dictionaryOccurencies: Seq[(String, Int)], cipherTextOccurrencies: Seq[(String, Int)],
                            dictionaryLength: Int, cipherTextLength: Int): Double ={
    var IMC = 0L
    var i = 0
    while(i < 26*26) {
      var fi=dictionaryOccurencies(i)._2
      var fi2=cipherTextOccurrencies(i)._2
      IMC += fi.toLong * fi2.toLong
      i+=1
    }
    var newIMC= IMC.toDouble/(dictionaryLength.toLong * cipherTextLength.toLong).toDouble
    return newIMC
  }

  def computeKeyMap(str: String) : mutable.HashMap[Char, Char]={
    var map = new mutable.HashMap[Char, Char]
    var intOfChar = 65
    for(c <- str){
      map.put(intOfChar.toChar, c)
      intOfChar +=1
    }
    return map
  }

  def computeDecryption(str: String, charToChar: mutable.HashMap[Char, Char]) : String ={
    var decryption = str.map(c => charToChar(c))
    return decryption
  }

  def main(args: Array[String]){
    var cipherTextMessagePath: String = args(0)
    var charPath: String = args(1)
    var digramsPath: String = args(2)
    var startingKey: String = args(3)
    var nIterations: Int = args(4).toInt

    var cipherTextMessage = Source.fromFile(cipherTextMessagePath).getLines.mkString

    //Carico la mappa di occorrenze del dizionario
    var dictionaryCharOccurenciesMap = new mutable.HashMap[Char, Int]
    for(line <- Source.fromFile(charPath).getLines) {
      var values: Array[String] = line.split(" ")
      dictionaryCharOccurenciesMap.put(values(0).charAt(0),values(1).toInt)
    }
    var dictionaryCharOccurencies = dictionaryCharOccurenciesMap.toSeq.sortBy(_._1)
    var dictionaryLength = computeLengthByOccurrencies(dictionaryCharOccurencies)

    var dictionaryDigramsOccurenciesMap = new mutable.HashMap[String, Int]
    for(line <- Source.fromFile(digramsPath).getLines) {
      var values: Array[String] = line.split(" ")
      dictionaryDigramsOccurenciesMap.put(values(0).charAt(0).toString+values(0).charAt(1).toString,values(1).toInt)
    }
    var dictionaryDigramsOccurencies = dictionaryDigramsOccurenciesMap.toSeq.sortBy(_._1)

    //Calcolo Indice Coincidenza del dizionario dei caratteri
    var dictionaryIC = computeIC(dictionaryDigramsOccurencies)

    var iteration = 0
    var previousKeyMap = computeKeyMap(startingKey)
    var actualKeyMap = new mutable.HashMap[Char, Char]
    for(c <- previousKeyMap){
      actualKeyMap.put(c._1, c._2)
    }
    var pos1 = 0
    var pos2 = 1
    var decryptionText = computeDecryption(cipherTextMessage, previousKeyMap)
    var decryptionDigramsOccurrencies = computeDigramsOccurrencies(decryptionText)
    var decryptionCharOccurrencies = computeCharOccurrencies(decryptionText)
    var previousIC = computeDigramsMIC(dictionaryDigramsOccurencies, decryptionDigramsOccurrencies, dictionaryLength, cipherTextLength = computeLengthByOccurrencies(decryptionCharOccurrencies))
    while(iteration < nIterations){
      actualKeyMap.empty
      for(c <- previousKeyMap){
        actualKeyMap.put(c._1, c._2)
      }
      var keySequence = actualKeyMap.toSeq.sortBy(_._1)
      var c1 = keySequence(pos1)._2
      var c2 = keySequence(pos2)._2
      actualKeyMap.put((pos1+65).toChar, c2)
      actualKeyMap.put((pos2+65).toChar, c1)
      var actualText = computeDecryption(cipherTextMessage, actualKeyMap)
      var actualDigramsOccurrencies = computeDigramsOccurrencies(actualText)
      var actualCharOccurrencies = computeCharOccurrencies(actualText)
      var actualIC = computeDigramsMIC(dictionaryDigramsOccurencies, actualDigramsOccurrencies, dictionaryLength, cipherTextLength = computeLengthByOccurrencies(actualCharOccurrencies))
      if(Math.abs(actualIC-dictionaryIC)<Math.abs(previousIC-dictionaryIC)){
        previousIC=actualIC
        previousKeyMap.empty
        for(c <- actualKeyMap){
          previousKeyMap.put(c._1, c._2)
        }
        pos1 = (pos1+1)%26
        if(pos1 == pos2){
          pos2 = (pos1+1)%26
        }
      }
      else{
        pos2 = (pos2+1)%26
        if(pos1 == pos2){
          pos1 = (pos1+1)%26
          pos2 = (pos1+1)%26
        }
      }
      iteration += 1
    }

    var decryptedText = computeDecryption(cipherTextMessage, previousKeyMap)
    println(s"Indice di coincidenza del dizionario fornito:\n$dictionaryIC")
    println(s"Indice di mutua coincidenza col dizionario fornito della traduzione trovata:\n$previousIC")
    println(s"Mappa relativa:\n$previousKeyMap")
    println(s"Traduzione trovata:\n$decryptedText")
  }
}
