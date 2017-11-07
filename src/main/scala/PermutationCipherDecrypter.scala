import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io._

object PermutationCipherDecrypter{
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

  def computeDigramsIC(occurrencies: Seq[(String, Int)]) : Double = {
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

  def computeLengthByOccurrencies(occurrencies: Seq[(Char, Int)]): Int ={
    var length=0
    for(element <- occurrencies){
      length += element._2
    }
    return length
  }

  def computeRandomKeyWithLenght(m :Int) :ListBuffer[Int] ={
    var key = new ListBuffer[Int]()

    for(i <- 0 until m){
      key+=m-1-i
    }

    for(i <- 0 until m*2){
      var r = scala.util.Random
      var v1 = r.nextInt(m)
      var v2 = r.nextInt(m)
      while(v1 == v2){
        v2 = r.nextInt(m)
      }
      var app = key(v1)
      key(v1) = key(v2)
      key(v2) = app
    }

    return key
  }

  def permutationDecryption(cipherText :String, key :ListBuffer[Int]) :String ={
    var plainText = ""

    for(i <- 0 until cipherText.length/key.length){
      for(j <- 0 until key.length){
        plainText+= cipherText.charAt(key.length*i+key(j)).toString
      }
    }

    return plainText
  }

  def main(args: Array[String]){
    var cipherTextMessagePath: String = args(0)
    var charactersPath: String = args(1)
    var digramsPath: String = args(2)
    var iterations: Int = args(3).toInt

    var cipherTextMessage = Source.fromFile(cipherTextMessagePath).getLines.mkString

    //Carico la mappa di occorrenze del dizionario
    var dictionaryCharOccurenciesMap = new mutable.HashMap[Char, Int]
    for(line <- Source.fromFile(charactersPath).getLines) {
      var values: Array[String] = line.split(" ")
      dictionaryCharOccurenciesMap.put(values(0).charAt(0),values(1).toInt)
    }
    var dictionaryCharOccurencies = dictionaryCharOccurenciesMap.toSeq.sortBy(_._1)

    var dictionaryDigramsOccurenciesMap = new mutable.HashMap[String, Int]
    for(line <- Source.fromFile(digramsPath).getLines) {
      var values: Array[String] = line.split(" ")
      dictionaryDigramsOccurenciesMap.put(values(0).charAt(0).toString+values(0).charAt(1).toString,values(1).toInt)
    }
    var dictionaryDigramsOccurencies = dictionaryDigramsOccurenciesMap.toSeq.sortBy(_._1)

    var M = -1
    var m = 11
    var MIC = -1.0D
    var dictionaryIC = computeDigramsIC(dictionaryDigramsOccurencies)
    var k  = ListBuffer(0)
    var cipherTextMessageLength = cipherTextMessage.length
    var dictionaryLength= computeLengthByOccurrencies(dictionaryCharOccurencies)

    while(m < cipherTextMessageLength){
      if(cipherTextMessageLength%m==0){
        println(dictionaryIC)
        println(m)
        var tk = computeRandomKeyWithLenght(m)
        var TMIC = -1.0D
        var pos1 = 0
        var pos2 = 1
        for(i <- 0 until iterations){
          var prevMIC = TMIC
          var temporaryDecryption = permutationDecryption(cipherTextMessage, tk)
          var temporaryDecryptionDigramOccurrencies = computeDigramsOccurrencies(cipherTextMessage)
          TMIC = computeDigramsIC(temporaryDecryptionDigramOccurrencies)
          //TMIC = computeDigramsMIC(dictionaryDigramsOccurencies, temporaryDecryptionDigramOccurrencies, dictionaryLength, cipherTextMessageLength)
          //tk = computeNewKey(tk, TMIC, prevMIC, dictionaryIC, pos1, pos2)
          //Compute NewKey
          if(Math.abs(TMIC-dictionaryIC)<Math.abs(prevMIC-dictionaryIC)){
            prevMIC=TMIC
            pos1 = (pos1+1)%m
            if(pos1 == pos2){
              pos2 = (pos1+1)%m
            }
          }
          else{
            pos2 = (pos2+1)%m
            if(pos1 == pos2){
              pos1 = (pos1+1)%m
              pos2 = (pos1+1)%m
            }
          }
          var app = tk(pos1)
          tk(pos1) = tk(pos2)
          tk(pos2) = app
        }
        if(Math.abs(dictionaryIC-MIC)>Math.abs(dictionaryIC-TMIC)){
          MIC = TMIC
          M = m
          k = tk
        }
      }
      m += 1
    }

    println(m)
    println(k)

    var plainText = permutationDecryption(cipherTextMessage, k)

    println(plainText)

  }
}
