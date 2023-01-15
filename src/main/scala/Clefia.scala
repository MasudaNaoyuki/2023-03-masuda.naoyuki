package clefia

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import clefia.Numeric._

/**
  * Created by gastonsantoalla on 31/10/16.
  */
class InvalidKeyException(message: String ) extends RuntimeException

trait Clefia {
  def main[T](blocks: Array[Numeric128], key: T): Unit = {
    var n = 0
    val start = System.currentTimeMillis
    val time = new Array[Long](11000)
    for (i <- 1 to 10000) {
      val start_n = System.currentTimeMillis
      encrypt(blocks, key)
      decrypt(encrypt(blocks, key), key)
      n = n + 1
      print(+n + "\n")
      val end_n = System.currentTimeMillis
      time(i + 1) = System.currentTimeMillis
      if (n == 10000) {
        System.out println ("N=" + n + ",time:" + (end_n - start_n) + " ms")
      } //System.out println("time:" + (end_n - start_n) + " ms")
    }
    val average = (time(10001) - start) / 10000
    System.out.println("total-time:" + (time(10001) - start) + "ms", "average-time:" + average + "ms")
  }

  def printDuration(timestamp: Long) = println(f"Duration: ${(System.nanoTime - timestamp) * 0.000000001}s")

  def getKeys[T](key: T): (Keys, Int) = key match  {
    case (k0: Int, k1: Int, k2: Int, k3: Int) => (KeyScheduling.scheduleKeys((k0, k1, k2, k3)), 18)
    case (k0: Int, k1: Int, k2: Int, k3: Int, k4: Int, k5: Int) => (KeyScheduling.scheduleKeys((k0, k1, k2, k3, k4, k5)), 22)
    case (k0: Int, k1: Int, k2: Int, k3: Int, k4: Int, k5: Int, k6: Int, k7: Int) => (KeyScheduling.scheduleKeys((k0, k1, k2, k3, k4, k5, k6, k7)), 26)
    case k: String => k.length match {
      case 16 => getKeys(k.toNumeric128)
      case 24 => getKeys(k.toNumeric192)
      case 32 => getKeys(k.toNumeric256)
      case l => throw new InvalidKeyException(s"Invalid Key $l is not a valid length fot a string key, only 16, 24 and 32 string length are supported")
    }
    case _ => throw new InvalidKeyException("Invalid Key, only 128, 192 and 256 bits keys are supported")
  }

  def getBlocks(a: Array[Byte]): Array[Numeric128] =
    a.grouped(4).map(b => ByteBuffer.wrap(Array[Byte](b(0), b(1), b(2), b(3))).getInt).grouped(4).map(_.toArray.toNumeric128).toArray

  def getByteArray(blocks: Array[Numeric128]): Array[Byte] =
    blocks.flatMap { b => ByteBuffer.allocate(16).putInt(b._1).putInt(b._2).putInt(b._3).putInt(b._4)array() }

  def process[T](blocks: Array[Numeric128], key: T, f: (Numeric128, Keys, Int) => Numeric128): Array[Numeric128]

  def processText[T](text: String, key: T, f: (Array[Numeric128], T) => Array[Numeric128]): String =
    f(text.toNumeric128Blocks, key).map(_.toRealString).mkString

  def processFile[T](originPath: String, destinationPath: String, key: T, operation: String, f: (Array[Byte], T) => Array[Byte]) = {
    val timestamp = System.nanoTime
    val finalPath = Paths.get(destinationPath)
    println(f"$operation: ${Files.write(finalPath, f(Files.readAllBytes(Paths.get(originPath)), key))}")
    printDuration(timestamp)
    finalPath.toString
  }

  def processFileExceptHeader[T](originPath: String, destinationPath: String, key: T, headerSize: Int, operation: String, f: (Array[Byte], T) => Array[Byte]) = {
    val timestamp = System.nanoTime
    val byteArray = Files.readAllBytes(Paths.get(originPath))
    val finalPath = Paths.get(destinationPath)
    val (header, body) = byteArray.splitAt(headerSize)
    println(f"$operation: ${Files.write(finalPath, header ++ f(body, key))}")
    printDuration(timestamp)
    finalPath.toString
  }

  def encrypt[T](blocks: Array[Numeric128], key: T): Array[Numeric128] = process(blocks, key, DataProcessing.enc)
  def decrypt[T](blocks: Array[Numeric128], key: T): Array[Numeric128] = process(blocks, key, DataProcessing.dec)

  def encryptBlock[T](block: Numeric128, key: T): Numeric128 = encrypt(Array(block), key).head
  def decryptBlock[T](block: Numeric128, key: T): Numeric128 = decrypt(Array(block), key).head

  def encryptText[T](plaintText: String, key: T): String = {
    val timestamp = System.nanoTime
    val mod = plaintText.length % 8
    val result = processText(plaintText + " " * (7 - mod) + mod, key, encrypt)
    printDuration(timestamp)
    result
  }

  def decryptText[T](cipherText: String, key: T): String = {
    val timestamp = System.nanoTime
    val decryptedString = processText(cipherText, key, decrypt)
    val result = decryptedString.dropRight(8 - decryptedString.last.asDigit)
    printDuration(timestamp)
    result
  }

  def encryptBytes[T](byteArray: Array[Byte], key: T): Array[Byte] = {
    val mod = byteArray.length % 16
    val paddedArray = byteArray ++ Array.fill(15 - mod)(0.toByte) :+ mod.toByte
    getByteArray(encrypt(getBlocks(paddedArray), key))
  }

  def decryptBytes[T](byteArray: Array[Byte], key: T): Array[Byte] = {
    val decryptedArray = getByteArray(decrypt(getBlocks(byteArray), key))
    decryptedArray.dropRight(16 - decryptedArray.last)
  }

  def encryptFile[T](originPath: String, destinationPath: String, key: T) = processFile(originPath, destinationPath, key, "Encrypted File", encryptBytes)
  def decryptFile[T](originPath: String, destinationPath: String, key: T) = processFile(originPath, destinationPath, key, "Decrypted File", decryptBytes)

  def encryptBMP[T](originPath: String, destinationPath: String, key: T) = processFileExceptHeader(originPath, destinationPath, key, 54, "Encrypted BMP", encryptBytes)
  def decryptBMP[T](originPath: String, destinationPath: String, key: T) = processFileExceptHeader(originPath, destinationPath, key, 54, "Decrypted BMP", decryptBytes)
}

object Clefia extends Clefia {
  def process[T](blocks: Array[Numeric128], key: T, f: (Numeric128, Keys, Int) => Numeric128 ): Array[Numeric128] = {
    //println("Begin Parallel Encryption")
    val (keys, rounds) = getKeys(key)
    //println("Keys Generated")
    blocks.par.map(f(_, keys, rounds)).toArray
  }
}

object ChainedClefia extends Clefia {
  def process[T](blocks: Array[Numeric128], key: T, f: (Numeric128, Keys, Int) => Numeric128): Array[Numeric128] = {
    println("Begin Chained Encryption")
    val blocksLength = blocks.length
    def singleProcess(numberBlock: Int, keys: Keys, rounds: Int, acc: Vector[Numeric128]): Vector[Numeric128] = {
      if (numberBlock == blocksLength) acc
      else {
        val processedElement = f(blocks(numberBlock), keys, rounds)
        print(s"Processed Blocks: $numberBlock/$blocksLength (${(numberBlock.toFloat /blocksLength*100).toInt}%)\r")
        val (sKeys, nRounds) = getKeys(processedElement ^ blocks(numberBlock))
        singleProcess(numberBlock + 1, sKeys, nRounds, acc :+ processedElement)
      }
    }

    val (keys, rounds) = getKeys(key)
    val result = singleProcess(0, keys, rounds, Vector()).toArray
    println(s"Processed Blocks: $blocksLength/$blocksLength (100%)")
    result
  }
}

