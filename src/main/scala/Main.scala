import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

object Main extends Methods {
  //def main(args:Array[String]): Unit = {
  def main[T](blocks: Array[Numeric128], key: T): Unit = {
    var n=0
    val start = System.currentTimeMillis
    val time = new Array[Long](6000)
    for (i <- 1 to 5000) {
      val start_n = System.currentTimeMillis
      encrypt(blocks,key)
      decrypt(encrypt(blocks,key),key)
      n=n+1
      print(+n+"\n")
      val end_n = System.currentTimeMillis
      time(i+1)=System.currentTimeMillis
      if(n==5000){
        System.out println("N="+n+",time:" + (end_n - start_n) + " ms")
      }//System.out println("time:" + (end_n - start_n) + " ms")
    }

    val average = (time(5001) - start)/5000
    //val sort_time =time.sorted
    //val average = average. (4, scala.math.BigDecimal.RoundingMode.HALF_UP)
    System.out.println("total-time:" + (time(5001) - start)+"ms","average-time:"+average+"ms")
  }
}
trait Methods {
  case class Keys(whiteningKeys: Numeric128, roundKeys: Array[Int])
  class InvalidKeyException(message: String ) extends RuntimeException
  /**
   * Numeric
   * */
  type Numeric32 = (Short, Short, Short, Short)
  type Numeric128 = (Int, Int, Int, Int)
  type Numeric192 = (Int, Int, Int, Int, Int, Int)
  type Numeric256 = (Int, Int, Int, Int, Int, Int, Int, Int)
  implicit class gf8Algebra(num: Int) {
    def gf8Multi(other: Int): Int = {
      @tailrec
      def rMult(acc: Int, v1: Int, v2: Int): Int =
        if (v2 == 0) acc & 255
        else {
          val nAcc = if ( (v2 & 1) !=0 ) acc ^ v1 else acc
          val nv1 = v1 << 1
          if ((nv1 & 256)!=0)
            rMult(nAcc, nv1 ^ 0x1d, v2 >>> 1)
          else
            rMult(nAcc, nv1, v2 >>> 1)
        }
      rMult(0, num, other)
    }
  }
  implicit class MatrixAlgebra(matrix: Array[Int]) {
    def squareMatrixXVector(vector: Array[Int]): Array[Short] = {
      val n = vector.length
      (0 until n map { i =>
        (0 until n).map(j =>
          matrix(i*n + j).gf8Multi(vector(j))).reduce(_^_)
      }).toArray.map(_.toShort)
    }
  }
  implicit class IntBytesOps(num: Int) {
    def getBytes: Numeric32 = Array(num >>> 24, num >>> 16, num >>> 8, num).map( b => (b & 0xff).toShort).toNumeric32
    def getChars: Array[Char] = Array(num >>> 16, num & 0xffff).map(_.toChar)
  }
  implicit class StringToNumerics(s: String) {
    def toIntArray: Array[Int] = s.toCharArray.map(_.toInt).grouped(2).toArray.map(_.concatChars)
    def toNumeric128: Numeric128 = s.toIntArray.toNumeric128
    def toNumeric192: Numeric192 = s.toIntArray.toNumeric192
    def toNumeric256: Numeric256 = s.toIntArray.toNumeric256
    def toNumeric128Blocks: Array[Numeric128] = s.grouped(8).map(_.toNumeric128).toArray
  }
  implicit class ShortArrayToNumerics(a: Array[Short]) {
    def toNumeric32: Numeric32 = (a(0), a(1), a(2), a(3))
  }
  implicit class IntArrayToNumerics(a: Array[Int]) {
    def toNumeric128: Numeric128 = (a(0), a(1), a(2), a(3))
    def toNumeric192: Numeric192 = (a(0), a(1), a(2), a(3), a(4), a(5))
    def toNumeric256: Numeric256 = (a(0), a(1), a(2), a(3), a(4), a(5), a(6), a(7))
    def concatChars: Int = a(0) << 16 | a(1)
  }
  implicit class Numeric32Ops(num: Numeric32) {
    def toArray: Array[Short] = Array(num._1, num._2, num._3 ,num._4)
    def concatBytes: Int =  num._1 << 24 | num._2 << 16 | num._3 << 8 | num._4
  }
  implicit class Numeric128Ops(num: Numeric128) {
    def ^(other: Numeric128): Numeric128 = (num._1 ^ other._1, num._2 ^ other._2, num._3 ^ other._3, num._4 ^ other._4)
    def toArray: Array[Int] = Array(num._1, num._2, num._3 ,num._4)
    def toRealString: String = num.toArray.flatMap(_.getChars).mkString
    def concat(other: Numeric128): Numeric256 = (num._1, num._2, num._3 ,num._4, other._1, other._2, other._3 , other._4)
  }
  implicit class Numeric256Ops(num: Numeric256) {
    def toArray: Array[Int] = Array(num._1, num._2, num._3 ,num._4, num._5, num._6, num._7, num._8)
    def first128: Numeric128 =  (num._1, num._2, num._3 ,num._4)
    def last128: Numeric128 =  (num._5, num._6, num._7, num._8)
  }
  def cal(x:Int, y:Int)={
    x+y
  }
  /**
   * GFN
   */
  val S0 = Array(0x57, 0x49, 0xd1, 0xc6, 0x2f, 0x33, 0x74, 0xfb,
      0x95, 0x6d, 0x82, 0xea, 0x0e, 0xb0, 0xa8, 0x1c,
      0x28, 0xd0, 0x4b, 0x92, 0x5c, 0xee, 0x85, 0xb1,
      0xc4, 0x0a, 0x76, 0x3d, 0x63, 0xf9, 0x17, 0xaf,
      0xbf, 0xa1, 0x19, 0x65, 0xf7, 0x7a, 0x32, 0x20,
      0x06, 0xce, 0xe4, 0x83, 0x9d, 0x5b, 0x4c, 0xd8,
      0x42, 0x5d, 0x2e, 0xe8, 0xd4, 0x9b, 0x0f, 0x13,
      0x3c, 0x89, 0x67, 0xc0, 0x71, 0xaa, 0xb6, 0xf5,
      0xa4, 0xbe, 0xfd, 0x8c, 0x12, 0x00, 0x97, 0xda,
      0x78, 0xe1, 0xcf, 0x6b, 0x39, 0x43, 0x55, 0x26,
      0x30, 0x98, 0xcc, 0xdd, 0xeb, 0x54, 0xb3, 0x8f,
      0x4e, 0x16, 0xfa, 0x22, 0xa5, 0x77, 0x09, 0x61,
      0xd6, 0x2a, 0x53, 0x37, 0x45, 0xc1, 0x6c, 0xae,
      0xef, 0x70, 0x08, 0x99, 0x8b, 0x1d, 0xf2, 0xb4,
      0xe9, 0xc7, 0x9f, 0x4a, 0x31, 0x25, 0xfe, 0x7c,
      0xd3, 0xa2, 0xbd, 0x56, 0x14, 0x88, 0x60, 0x0b,
      0xcd, 0xe2, 0x34, 0x50, 0x9e, 0xdc, 0x11, 0x05,
      0x2b, 0xb7, 0xa9, 0x48, 0xff, 0x66, 0x8a, 0x73,
      0x03, 0x75, 0x86, 0xf1, 0x6a, 0xa7, 0x40, 0xc2,
      0xb9, 0x2c, 0xdb, 0x1f, 0x58, 0x94, 0x3e, 0xed,
      0xfc, 0x1b, 0xa0, 0x04, 0xb8, 0x8d, 0xe6, 0x59,
      0x62, 0x93, 0x35, 0x7e, 0xca, 0x21, 0xdf, 0x47,
      0x15, 0xf3, 0xba, 0x7f, 0xa6, 0x69, 0xc8, 0x4d,
      0x87, 0x3b, 0x9c, 0x01, 0xe0, 0xde, 0x24, 0x52,
      0x7b, 0x0c, 0x68, 0x1e, 0x80, 0xb2, 0x5a, 0xe7,
      0xad, 0xd5, 0x23, 0xf4, 0x46, 0x3f, 0x91, 0xc9,
      0x6e, 0x84, 0x72, 0xbb, 0x0d, 0x18, 0xd9, 0x96,
      0xf0, 0x5f, 0x41, 0xac, 0x27, 0xc5, 0xe3, 0x3a,
      0x81, 0x6f, 0x07, 0xa3, 0x79, 0xf6, 0x2d, 0x38,
      0x1a, 0x44, 0x5e, 0xb5, 0xd2, 0xec, 0xcb, 0x90,
      0x9a, 0x36, 0xe5, 0x29, 0xc3, 0x4f, 0xab, 0x64,
      0x51, 0xf8, 0x10, 0xd7, 0xbc, 0x02, 0x7d, 0x8e)
  private val S1 = Array(0x6c, 0xda, 0xc3, 0xe9, 0x4e, 0x9d, 0x0a, 0x3d,
      0xb8, 0x36, 0xb4, 0x38, 0x13, 0x34, 0x0c, 0xd9,
      0xbf, 0x74, 0x94, 0x8f, 0xb7, 0x9c, 0xe5, 0xdc,
      0x9e, 0x07, 0x49, 0x4f, 0x98, 0x2c, 0xb0, 0x93,
      0x12, 0xeb, 0xcd, 0xb3, 0x92, 0xe7, 0x41, 0x60,
      0xe3, 0x21, 0x27, 0x3b, 0xe6, 0x19, 0xd2, 0x0e,
      0x91, 0x11, 0xc7, 0x3f, 0x2a, 0x8e, 0xa1, 0xbc,
      0x2b, 0xc8, 0xc5, 0x0f, 0x5b, 0xf3, 0x87, 0x8b,
      0xfb, 0xf5, 0xde, 0x20, 0xc6, 0xa7, 0x84, 0xce,
      0xd8, 0x65, 0x51, 0xc9, 0xa4, 0xef, 0x43, 0x53,
      0x25, 0x5d, 0x9b, 0x31, 0xe8, 0x3e, 0x0d, 0xd7,
      0x80, 0xff, 0x69, 0x8a, 0xba, 0x0b, 0x73, 0x5c,
      0x6e, 0x54, 0x15, 0x62, 0xf6, 0x35, 0x30, 0x52,
      0xa3, 0x16, 0xd3, 0x28, 0x32, 0xfa, 0xaa, 0x5e,
      0xcf, 0xea, 0xed, 0x78, 0x33, 0x58, 0x09, 0x7b,
      0x63, 0xc0, 0xc1, 0x46, 0x1e, 0xdf, 0xa9, 0x99,
      0x55, 0x04, 0xc4, 0x86, 0x39, 0x77, 0x82, 0xec,
      0x40, 0x18, 0x90, 0x97, 0x59, 0xdd, 0x83, 0x1f,
      0x9a, 0x37, 0x06, 0x24, 0x64, 0x7c, 0xa5, 0x56,
      0x48, 0x08, 0x85, 0xd0, 0x61, 0x26, 0xca, 0x6f,
      0x7e, 0x6a, 0xb6, 0x71, 0xa0, 0x70, 0x05, 0xd1,
      0x45, 0x8c, 0x23, 0x1c, 0xf0, 0xee, 0x89, 0xad,
      0x7a, 0x4b, 0xc2, 0x2f, 0xdb, 0x5a, 0x4d, 0x76,
      0x67, 0x17, 0x2d, 0xf4, 0xcb, 0xb1, 0x4a, 0xa8,
      0xb5, 0x22, 0x47, 0x3a, 0xd5, 0x10, 0x4c, 0x72,
      0xcc, 0x00, 0xf9, 0xe0, 0xfd, 0xe2, 0xfe, 0xae,
      0xf8, 0x5f, 0xab, 0xf1, 0x1b, 0x42, 0x81, 0xd6,
      0xbe, 0x44, 0x29, 0xa6, 0x57, 0xb9, 0xaf, 0xf2,
      0xd4, 0x75, 0x66, 0xbb, 0x68, 0x9f, 0x50, 0x02,
      0x01, 0x3c, 0x7f, 0x8d, 0x1a, 0x88, 0xbd, 0xac,
      0xf7, 0xe4, 0x79, 0x96, 0xa2, 0xfc, 0x6d, 0xb2,
      0x6b, 0x03, 0xe1, 0x2e, 0x7d, 0x14, 0x95, 0x1d)

  val M0 = Array(0x01, 0x02, 0x04, 0x06,
      0x02, 0x01, 0x06, 0x04,
      0x04, 0x06, 0x01, 0x02,
      0x06, 0x04, 0x02, 0x01)

  val M1 = Array(0x01, 0x08, 0x02, 0x0a,
      0x08, 0x01, 0x0a, 0x02,
      0x02, 0x0a, 0x01, 0x08,
      0x0a, 0x02, 0x08, 0x01)

  def getTValues(key: Int, block: Int): Numeric32 = (key ^ block).getBytes

  def f0(key: Int, block: Int): Int= {
    val (t0, t1, t2, t3) = getTValues(key, block)
    M0.squareMatrixXVector(Array(S0(t0), S1(t1), S0(t2), S1(t3))).toNumeric32.concatBytes
  }

  def f1(key: Int, block: Int): Int = {
    val (t0, t1, t2, t3) = getTValues(key, block)
    M1.squareMatrixXVector(Array(S1(t0), S0(t1), S1(t2), S0(t3))).toNumeric32.concatBytes
    }

  //Harcoded GFNs exactly as the refference text
  def gfn4H(input: Numeric128, roundKeys:Array[Int], rounds: Int): Numeric128 = {
    @tailrec
    def shuffle(t: Numeric128, i: Int): Numeric128 = {
      val (t0, t1, t2, t3) = t

      if (i == rounds) (t3, t0, t1, t2)
      else shuffle((t1 ^ f0(roundKeys(2 * i), t0), t2, t3 ^ f1(roundKeys(2 * i + 1), t2), t0), i + 1)
    }

    shuffle(input, 0)
  }

  def gfn8H(input: Numeric256, roundKeys:Array[Int], rounds: Int): Numeric256 = {
    @tailrec
    def shuffle(t: Numeric256, i: Int): Numeric256 = {
      val (t0, t1, t2, t3, t4, t5, t6, t7) = t
      if (i == rounds) (t7, t0, t1, t2, t3, t4, t5, t6)
      else shuffle((t1 ^ f0(roundKeys(4 * i), t0), t2, t3 ^ f1(roundKeys(4 * i + 1), t2), t4, t5 ^ f0(roundKeys(4 * i + 2), t4), t6, t7 ^ f1(roundKeys(4 * i + 3), t6), t0), i + 1)
    }
    shuffle(input, 0)
  }
  //Generic gfn
  def gfn(input: Array[Int], roundKeys:Array[Int], rounds: Int): Array[Int] = {
    val factor = input.length / 2
    @tailrec
    def shuffle(t: Array[Int], i: Int): Array[Int] =
      if (i == rounds) t.last +: t.init
      else {
        val newT = t.zipWithIndex.map { case (v, index) =>
          if(index % 2 == 0) v
          else {
            val position = index / 2
            if (position % 2 == 0) v ^ f0(roundKeys(factor * i + position), t(index - 1)) else v ^ f1(roundKeys(factor * i + position), t(index - 1))
          }
        }
        shuffle(newT.tail :+ newT.head, i + 1)
      }
    shuffle(input, 0)
  }
  def gfn4(input: Numeric128, roundKeys:Array[Int], rounds: Int): Numeric128 = gfn(input.toArray, roundKeys, rounds).toNumeric128
  def gfn8(input: Numeric256, roundKeys:Array[Int], rounds: Int): Numeric256 = gfn(input.toArray, roundKeys, rounds).toNumeric256
  def gfn4Inverse(input: Numeric128, roundKeys:Array[Int], rounds: Int): Numeric128 = {
    @tailrec
    def shuffle(t: Numeric128, i: Int): Numeric128 = {
      val (t0, t1, t2, t3) = t
      if (i == rounds) (t1, t2, t3, t0)
      else shuffle((t3 ^ f1(roundKeys(2 * (rounds - i) - 1), t2), t0, t1 ^ f0(roundKeys(2 * (rounds - i) - 2), t0), t2), i + 1)
    }
    shuffle(input, 0)
  }
  /**
   * KeyScheduling
   */
  private val CON128 = Array(0xf56b7aeb, 0x994a8a42, 0x96a4bd75, 0xfa854521,
    0x735b768a, 0x1f7abac4, 0xd5bc3b45, 0xb99d5d62,
    0x52d73592, 0x3ef636e5, 0xc57a1ac9, 0xa95b9b72,
    0x5ab42554, 0x369555ed, 0x1553ba9a, 0x7972b2a2,
    0xe6b85d4d, 0x8a995951, 0x4b550696, 0x2774b4fc,
    0xc9bb034b, 0xa59a5a7e, 0x88cc81a5, 0xe4ed2d3f,
    0x7c6f68e2, 0x104e8ecb, 0xd2263471, 0xbe07c765,
    0x511a3208, 0x3d3bfbe6, 0x1084b134, 0x7ca565a7,
    0x304bf0aa, 0x5c6aaa87, 0xf4347855, 0x9815d543,
    0x4213141a, 0x2e32f2f5, 0xcd180a0d, 0xa139f97a,
    0x5e852d36, 0x32a464e9, 0xc353169b, 0xaf72b274,
    0x8db88b4d, 0xe199593a, 0x7ed56d96, 0x12f434c9,
    0xd37b36cb, 0xbf5a9a64, 0x85ac9b65, 0xe98d4d32,
    0x7adf6582, 0x16fe3ecd, 0xd17e32c1, 0xbd5f9f66,
    0x50b63150, 0x3c9757e7, 0x1052b098, 0x7c73b3a7)

  private val CON192 = Array(0xc6d61d91, 0xaaf73771, 0x5b6226f8, 0x374383ec,
    0x15b8bb4c, 0x799959a2, 0x32d5f596, 0x5ef43485,
    0xf57b7acb, 0x995a9a42, 0x96acbd65, 0xfa8d4d21,
    0x735f7682, 0x1f7ebec4, 0xd5be3b41, 0xb99f5f62,
    0x52d63590, 0x3ef737e5, 0x1162b2f8, 0x7d4383a6,
    0x30b8f14c, 0x5c995987, 0x2055d096, 0x4c74b497,
    0xfc3b684b, 0x901ada4b, 0x920cb425, 0xfe2ded25,
    0x710f7222, 0x1d2eeec6, 0xd4963911, 0xb8b77763,
    0x524234b8, 0x3e63a3e5, 0x1128b26c, 0x7d09c9a6,
    0x309df106, 0x5cbc7c87, 0xf45f7883, 0x987ebe43,
    0x963ebc41, 0xfa1fdf21, 0x73167610, 0x1f37f7c4,
    0x01829338, 0x6da363b6, 0x38c8e1ac, 0x54e9298f,
    0x246dd8e6, 0x484c8c93, 0xfe276c73, 0x9206c649,
    0x9302b639, 0xff23e324, 0x7188732c, 0x1da969c6,
    0x00cd91a6, 0x6cec2cb7, 0xec7748d3, 0x8056965b,
    0x9a2aa469, 0xf60bcb2d, 0x751c7a04, 0x193dfdc2,
    0x02879532, 0x6ea666b5, 0xed524a99, 0x8173b35a,
    0x4ea00d7c, 0x228141f9, 0x1f59ae8e, 0x7378b8a8,
    0xe3bd5747, 0x8f9c5c54, 0x9dcfaba3, 0xf1ee2e2a,
    0xa2f6d5d1, 0xced71715, 0x697242d8, 0x055393de,
    0x0cb0895c, 0x609151bb, 0x3e51ec9e, 0x5270b089)

  private val CON256 = Array(0x0221947e, 0x6e00c0b5, 0xed014a3f, 0x8120e05a,
    0x9a91a51f, 0xf6b0702d, 0xa159d28f, 0xcd78b816,
    0xbcbde947, 0xd09c5c0b, 0xb24ff4a3, 0xde6eae05,
    0xb536fa51, 0xd917d702, 0x62925518, 0x0eb373d5,
    0x094082bc, 0x6561a1be, 0x3ca9e96e, 0x5088488b,
    0xf24574b7, 0x9e64a445, 0x9533ba5b, 0xf912d222,
    0xa688dd2d, 0xcaa96911, 0x6b4d46a6, 0x076cacdc,
    0xd9b72353, 0xb596566e, 0x80ca91a9, 0xeceb2b37,
    0x786c60e4, 0x144d8dcf, 0x043f9842, 0x681edeb3,
    0xee0e4c21, 0x822fef59, 0x4f0e0e20, 0x232feff8,
    0x1f8eaf20, 0x73af6fa8, 0x37ceffa0, 0x5bef2f80,
    0x23eed7e0, 0x4fcf0f94, 0x29fec3c0, 0x45df1f9e,
    0x2cf6c9d0, 0x40d7179b, 0x2e72ccd8, 0x42539399,
    0x2f30ce5c, 0x4311d198, 0x2f91cf1e, 0x43b07098,
    0xfbd9678f, 0x97f8384c, 0x91fdb3c7, 0xfddc1c26,
    0xa4efd9e3, 0xc8ce0e13, 0xbe66ecf1, 0xd2478709,
    0x673a5e48, 0x0b1bdbd0, 0x0b948714, 0x67b575bc,
    0x3dc3ebba, 0x51e2228a, 0xf2f075dd, 0x9ed11145,
    0x417112de, 0x2d5090f6, 0xcca9096f, 0xa088487b,
    0x8a4584b7, 0xe664a43d, 0xa933c25b, 0xc512d21e,
    0xb888e12d, 0xd4a9690f, 0x644d58a6, 0x086cacd3,
    0xde372c53, 0xb216d669, 0x830a9629, 0xef2beb34,
    0x798c6324, 0x15ad6dce, 0x04cf99a2, 0x68ee2eb3)

  def doubleSwap(x: Numeric128): Numeric128 = {
    val (n0, n1, n2, n3) = x
    (((n0 << 7) & 0xffffff80) | (n1 >>> 25), //n0.last25 - n1.firstseven
      ((n1 << 7) & 0xffffff80) | (n3 & 0x7f),  //n1.last25 - n3.last7
      (n0 & 0xfe000000) | (n2 >>> 7),          //n0.first7 - n2.first25
      ((n2 << 25) & 0xfe000000) | (n3 >>> 7))  //n2.last7  - n3.first25
  }

  def scheduleKeys(baseKey: Numeric128): Keys = {
    @tailrec
    def loop(l: Numeric128, i: Int, acc: Array[Int]): Array[Int] =
      if (i == 9) acc
      else {
        val t = l ^ (CON128(24 + 4 * i), CON128(24 + 4 * i + 1), CON128(24 + 4 * i  + 2), CON128(24 + 4 * i  + 3))
        val rks = if(i % 2 == 0) t else t ^ baseKey

        loop(doubleSwap(l), i + 1, acc ++ rks.toArray)
      }

    Keys(baseKey, loop(gfn4(baseKey, CON128.slice(0, 24), 12), 0, Array()))
  }

  private def fromMoreThan128(con: Array[Int], threshold: Int, kl: Numeric128, kr: Numeric128) = {
    val l = gfn8(kl.concat(kr), con.slice(0, 40), 10)

    @tailrec
    def loop(ll: Numeric128, lr: Numeric128, i: Int, acc: Array[Int]): Array[Int] =
      if (i == threshold) acc
      else {
        val (workerL, workerKey, nll, nlr) = if (List(1,0).contains(i % 4)) (ll, kr, doubleSwap(ll), lr)
        else  (lr, kl, ll, doubleSwap(lr))

        val t = workerL ^ (con(40 + 4 * i), con(40 + 4 * i + 1), con(40 + 4 * i  + 2), con(40 + 4 * i  + 3))
        val rks = if(i % 2 == 0) t else t ^ workerKey

        loop(nll, nlr, i + 1, acc ++ rks.toArray)
      }

    Keys(kl ^ kr, loop(l.first128, l.last128, 0, Array()))
  }

  def scheduleKeys(baseKey: Numeric192): Keys = {
    val (k0, k1, k2, k3, k4, k5) = baseKey
    val (kl, kr) = ((k0, k1, k2, k3), (k4, k5, ~k0, ~k1))

    fromMoreThan128(CON192, 11, kl, kr)
  }

  def scheduleKeys(baseKey: Numeric256): Keys = {
    val (kl, kr) = (baseKey.first128, baseKey.last128)
    fromMoreThan128(CON256, 13, kl, kr)
  }


  /**
   * DataProcessing
   */
  def enc(plainText: Numeric128, keys: Keys, rounds: Int): Numeric128 = {
    val (p0, p1, p2, p3) = plainText
    val (wk0, wk1, wk2, wk3) = keys.whiteningKeys

    val (t0, t1, t2, t3) = gfn4((p0, p1 ^ wk0, p2, p3 ^ wk1), keys.roundKeys, rounds)
    (t0, t1 ^ wk2, t2, t3 ^ wk3)
  }

  def dec(cipherText: Numeric128, keys: Keys, rounds: Int): Numeric128 = {
    val (c0, c1, c2, c3) = cipherText
    val (wk0, wk1, wk2, wk3) = keys.whiteningKeys

    val (t0, t1, t2, t3) = gfn4Inverse((c0, c1 ^ wk2, c2, c3 ^ wk3), keys.roundKeys, rounds)
    (t0, t1 ^ wk0, t2, t3 ^ wk1)
  }

  /**
   *
   */
  private def printDuration(timestamp: Long): Unit = println(f"Duration: ${(System.nanoTime - timestamp) * 0.000000001}s")

  def getKeys[T](key: T): (Keys, Int) = key match  {
    case (k0: Int, k1: Int, k2: Int, k3: Int) => (scheduleKeys((k0, k1, k2, k3)), 18)
    case (k0: Int, k1: Int, k2: Int, k3: Int, k4: Int, k5: Int) => (scheduleKeys((k0, k1, k2, k3, k4, k5)), 22)
    case (k0: Int, k1: Int, k2: Int, k3: Int, k4: Int, k5: Int, k6: Int, k7: Int) => (scheduleKeys((k0, k1, k2, k3, k4, k5, k6, k7)), 26)
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
    blocks.flatMap { b => ByteBuffer.allocate(16).putInt(b._1).putInt(b._2).putInt(b._3).putInt(b._4).array() }

  //def process[T](blocks: Array[Numeric128], key: T, f: (Numeric128, Keys, Int) => Numeric128): Array[Numeric128]

  def processText[T](text: String, key: T, f: (Array[Numeric128], T) => Array[Numeric128]): String =
    f(text.toNumeric128Blocks, key).map(_.toRealString).mkString

  private def processFile[T](originPath: String, destinationPath: String, key: T, operation: String, f: (Array[Byte], T) => Array[Byte]): String = {
    val timestamp = System.nanoTime
    val finalPath = Paths.get(destinationPath)
    println(f"$operation: ${Files.write(finalPath, f(Files.readAllBytes(Paths.get(originPath)), key))}")
    printDuration(timestamp)
    finalPath.toString
  }

  private def processFileExceptHeader[T](originPath: String, destinationPath: String, key: T, headerSize: Int, operation: String, f: (Array[Byte], T) => Array[Byte]) = {
    val timestamp = System.nanoTime
    val byteArray = Files.readAllBytes(Paths.get(originPath))
    val finalPath = Paths.get(destinationPath)
    val (header, body) = byteArray.splitAt(headerSize)
    println(f"$operation: ${Files.write(finalPath, header ++ f(body, key))}")
    printDuration(timestamp)
    finalPath.toString
  }


  def encrypt[T](blocks: Array[Numeric128], key: T): Array[Numeric128] = process(blocks, key, enc)
  def decrypt[T](blocks: Array[Numeric128], key: T): Array[Numeric128] = process(blocks, key, dec)

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

  ///
  def process[T](blocks: Array[Numeric128], key: T, f: (Numeric128, Keys, Int) => Numeric128): Array[Numeric128] = {
    val blocksLength = blocks.length
    @tailrec
    def singleProcess(numberBlock: Int, keys: Keys, rounds: Int, acc: Vector[Numeric128]): Vector[Numeric128] = {
      if (numberBlock == blocksLength) acc
      else {
        val processedElement = f(blocks(numberBlock), keys, rounds)
        //print(s"Processed Blocks: $numberBlock/$blocksLength (${(numberBlock.toFloat /blocksLength*100).toInt}%)\r")
        val (sKeys, nRounds) = getKeys(processedElement ^ blocks(numberBlock))
        singleProcess(numberBlock + 1, sKeys, nRounds, acc :+ processedElement)
      }
    }

    val (keys, rounds) = getKeys(key)
    val result = singleProcess(0, keys, rounds, Vector()).toArray
    //val result = process(blocks, key, enc)
    result
  }
}
