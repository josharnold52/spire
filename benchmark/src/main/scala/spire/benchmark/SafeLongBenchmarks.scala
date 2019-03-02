package spire
package benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import spire.math.SafeLong
import SafeLongUtil._
import spire.random.rng.MersenneTwister64

object SafeLongUtil {
  private def isBig(x: SafeLong) = x.getClass.getSimpleName.endsWith("BigInteger")

  def classify(a: SafeLong): String =
    if (isBig(a)) "b" else "l"

  def classify(a: SafeLong, op_a: SafeLong): String =
    classify(a) + "_" + classify(op_a)

  def classify(a: SafeLong, b: SafeLong, a_op_b: SafeLong): String =
    classify(a) + "_" + classify(b) + "_" + classify(a_op_b)

  def check(cases: Map[String, (SafeLong, SafeLong)]): Unit = {
    for ((kind, (a, b)) ← cases) {
      val c = classify(a, b)
      require(kind.startsWith(c), s"Unexpected class $c for case $kind")
    }
  }

  def check(cases: Map[String, (SafeLong, SafeLong)], op: (SafeLong, SafeLong) ⇒ SafeLong): Unit = {
    for ((kind, (a, b)) ← cases) {
      val c = classify(a, b, op(a, b))
      require(kind.startsWith(c), s"Unexpected class $c for case $kind")
    }
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongMultiplyBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l_l" → ((SafeLong.two, SafeLong.two)),
    "l_l_b" → ((SafeLong.two, SafeLong.safe64 - 1)),
    "l_b_l" → ((SafeLong.minusOne, SafeLong.safe64)),
    "l_b_b" → ((SafeLong.two, SafeLong.safe64)),
    "b_l_l" → ((SafeLong.safe64, SafeLong.minusOne)),
    "b_l_b" → ((SafeLong.safe64, SafeLong.two)),
    "b_b_b" → ((SafeLong.safe64, SafeLong.safe64)),
    "l_l_l_cm1" → ((SafeLong.two, SafeLong(Int.MaxValue.toLong << 1))),  //Checked multiplication slowpath
    "l_l_l_cm2" → ((SafeLong(Int.MaxValue.toLong << 1), SafeLong.two))   //Checked multiplication slowpath
  )
  check(pairs, _ * _)

  @Param(Array("l_l_l", "l_l_b", "l_b_l", "l_b_b", "b_l_l", "b_l_b", "b_b_b", "l_l_l_cm1", "l_l_l_cm2"))
  var kind: String = ""

  var a: SafeLong = 0L

  var b: SafeLong = 0L

  var c: SafeLong = 0L

  @Setup
  def setup(): Unit = {
    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
    c = -b0
  }

  @Benchmark
  def multiply(x: Blackhole): Unit = {
    x.consume(a * b)
  }
}



@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class SafeLongMixedMultiplyBenchmark {


  @Param(Array("0.0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1.0"))
  var fastPathFraction: String = _

  @Param(Array("l_l_l_rs","l_l_l_sr","l_l_b_ss"))
  var slowPathKey: String = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    selections = SafeLongMixedMultiplyBenchmark.selections(fastPathFraction.toDouble)
    val (_as,_bs) = SafeLongMixedMultiplyBenchmark.unzippedPairs(slowPathKey)
    as = _as
    bs = _bs
  }

  private[this] var selections: Array[Byte] = null
  private[this] var as: Array[SafeLong] = _
  private[this] var bs: Array[SafeLong] = _

  @Benchmark
  def multiply(index: SafeLongMixedMultiplyBenchmark.Index, x: Blackhole): Unit = {
    val i = index.index
    val i2 = selections(i)
    val a = as(i2)
    val b = bs(i2)
    val nxi = i + 1
    index.index = if (nxi < selections.length) nxi else 0
    x.consume(a * b)
  }
}

object SafeLongMixedMultiplyBenchmark {
  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l_l" → ((SafeLong.two, SafeLong.two)) ,
    "l_l_l_rs" → ((SafeLong.two, SafeLong(Int.MaxValue.toLong << 1))), //Checked multiplication slowpath
    "l_l_l_sr" → ((SafeLong(Int.MaxValue.toLong << 1), SafeLong.two)), //Checked multiplication slowpath
    "l_l_b_ss"→ ((SafeLong(Int.MaxValue.toLong << 1), (SafeLong(Int.MaxValue.toLong << 1))) //Promotion
      ))
  check(pairs, _ * _)


  @State(Scope.Thread)
  class Index {
    var index: Int = 0
  }
  final val orderSize = 8721

  def unzippedPairs(key: String): (Array[SafeLong],Array[SafeLong]) =
    Array("l_l_l",key).map(pairs).unzip

  def selections(fastPathFraction: Double): Array[Byte] = {
    val r = MersenneTwister64.fromTime(7166321 * java.lang.Double.doubleToLongBits(fastPathFraction))
    Array.fill[Byte](orderSize)({
      val nr = r.nextDouble()
      if (nr < fastPathFraction) 0 else 1
    })
  }


}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongAddSubtractBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l_l" → ((SafeLong.one, SafeLong.one)),
    "l_l_b" → ((SafeLong.one, SafeLong.safe64 - 1)),
    "l_b_l" → ((SafeLong.minusOne, SafeLong.safe64)),
    "l_b_b" → ((SafeLong.one, SafeLong.safe64)),
    "b_l_l" → ((SafeLong.safe64, SafeLong.minusOne)),
    "b_l_b" → ((SafeLong.safe64, SafeLong.one)),
    "b_b_l" → ((SafeLong.safe64, -SafeLong.safe64 - 1)),
    "b_b_b" → ((SafeLong.safe64, SafeLong.safe64))
  )
  check(pairs, _ + _)
  check(pairs, _ - -_)

  @Param(Array("l_l_l", "l_l_b", "l_b_l", "l_b_b", "b_l_l", "b_l_b", "b_b_l", "b_b_b"))
  var kind: String = ""
  var a: SafeLong = 0L
  var b: SafeLong = 0L
  var c: SafeLong = 0L

  @Setup
  def setup(): Unit = {


    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
    c = -b0
  }

  @Benchmark
  def add(x: Blackhole): Unit = {
    x.consume(a + b)
  }

  def subtract(x: Blackhole): Unit = {
    x.consume(a - c)
  }
}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class SafeLongCompareBenchmark {

  val pairs: Map[String, (SafeLong, SafeLong)] = Map(
    "l_l" → ((SafeLong.one, SafeLong.one + 1)),
    "b_b" → ((SafeLong.safe64, SafeLong.safe64 + 1))
  )
  check(pairs)

  @Param(Array("l_l", "b_b"))
  var kind: String = ""
  var a: SafeLong = 0L
  var b: SafeLong = 0L

  @Setup
  def setup(): Unit = {


    val (a0, b0) = pairs(kind)
    a = a0
    b = b0
  }

  @Benchmark
  def compare(x: Blackhole): Unit = {
    x.consume(a compare b)
  }
}
