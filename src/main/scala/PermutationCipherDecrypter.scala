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

  def computeLengthByOccurrencies(occurrencies: Seq[(Char, Int)]): Int ={
    var length=0
    for(element <- occurrencies){
      length += element._2
    }
    return length
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
    var m = 2
    var MIC = -1.0D
    var k  = new ListBuffer[Int]()
    var cipherTextMessageLength = cipherTextMessage.length
    var dictionaryLength= computeLengthByOccurrencies(dictionaryCharOccurencies)
    var plainText = ""

    var pyupypu = Array.ofDim[Int](26,26)
    for(i <- 0 until 26){
      for(j <- 0 until 26){
        var v1:Float = 0f
        if(dictionaryCharOccurencies(i)._2 != 0 && dictionaryCharOccurencies(j)._2 !=0) {
          v1 = (dictionaryDigramsOccurencies(i * 26 + j)._2.toFloat / dictionaryLength.toFloat) /
            ((dictionaryCharOccurencies(i)._2.toFloat / dictionaryLength.toFloat) * (dictionaryCharOccurencies(j)._2.toFloat / dictionaryLength.toFloat))
        }
        var v2:Int = 0
        if(v1 < 1.19 && v1 > 1/1.19){
          v2 = 0
        }else if(v1 >= 13.5){
          v2 = 8
        }else if(v1 <= 1/13.5){
          v2 = -8
        }else if(v1 > 1.19){
          v2 = (v1*7/13.5).toInt
        }else if(v1 < 1/1.19){
          v2 = ((v1-1/13.5f)/((1/1.19f-1/13.5)/7f)-8f).toInt
        }
        pyupypu(i)(j)=v2
      }
    }

    var ans= ""
    while(m < cipherTextMessageLength){
      k = new ListBuffer[Int]()
      var sumpMatrix = Array.ofDim[Float](m,m)
      if(cipherTextMessageLength%m==0){
        println(m)
        for(i <- 0 until m){
          for(j <- 0 until m){
            if(i == j) {
              sumpMatrix(i)(j)= Float.MinValue
            }else {
              var sum:Float = 0.0f
              for(z <- 0 until cipherTextMessageLength/m) {
                sum+= pyupypu(cipherTextMessage.charAt(i*z).toInt-65)(cipherTextMessage.charAt(j*z).toInt-65)
              }
              sum /= cipherTextMessageLength/m
              sumpMatrix(i)(j)= sum
            }
          }
        }
        for(i <- 0 until m){
          var app = Float.MinValue
          var indexu = 0
          for(j <- 0 until m){
            if(sumpMatrix(i)(j)>app){
              app = sumpMatrix(i)(j)
              indexu = j
            }
          }
          k.append(indexu)
        }

        plainText = permutationDecryption(cipherTextMessage, k)
        println(s"$plainText\n")
        println("La traduzione è accettabile?(si/no)\n")
        ans = scala.io.StdIn.readLine()
        if(ans=="si"){
          m = cipherTextMessageLength
        }
      }
      if(m > 3000){
        m = cipherTextMessageLength
      }else{
        m += 1
      }
    }

    if(ans == "si") {
      println(M)
      println(k)
      println(plainText)
    }else{
      println("Chiave non trovata")
    }

  }
}