package scalax.concurrent.atomic.padded

import scalax.concurrent.atomic.AtomicNumberSuite

object AtomicDoubleSuite extends AtomicNumberSuite[Double, AtomicDouble](
  "AtomicDouble", Atomic.builderFor(0.0), 17.23, Double.MaxValue, Double.MinValue) {

  test("should store MinPositiveValue, NaN, NegativeInfinity, PositiveInfinity") {
    assert(Atomic(Double.MinPositiveValue).get == Double.MinPositiveValue)
    assert(Atomic(Double.NaN).get.isNaN)
    assert(Atomic(Double.NegativeInfinity).get.isNegInfinity)
    assert(Atomic(Double.PositiveInfinity).get.isPosInfinity)
  }
}

object AtomicFloatSuite extends AtomicNumberSuite[Float, AtomicFloat](
  "AtomicFloat", Atomic.builderFor(0.0f), 17.23f, Float.MaxValue, Float.MinValue) {

  test("should store MinPositiveValue, NaN, NegativeInfinity, PositiveInfinity") {
    assert(Atomic(Float.MinPositiveValue).get == Float.MinPositiveValue)
    assert(Atomic(Float.NaN).get.isNaN)
    assert(Atomic(Float.NegativeInfinity).get.isNegInfinity)
    assert(Atomic(Float.PositiveInfinity).get.isPosInfinity)
  }
}

object AtomicLongSuite extends AtomicNumberSuite[Long, AtomicLong](
  "AtomicLong", Atomic.builderFor(0L), -782L, Long.MaxValue, Long.MinValue)

object AtomicIntSuite extends AtomicNumberSuite[Int, AtomicInt](
  "AtomicInt", Atomic.builderFor(0), 782, Int.MaxValue, Int.MinValue)

object AtomicShortSuite extends AtomicNumberSuite[Short, AtomicShort](
  "AtomicShort", Atomic.builderFor(0.toShort), 782.toShort, Short.MaxValue, Short.MinValue)

object AtomicByteSuite extends AtomicNumberSuite[Byte, AtomicByte](
  "AtomicByte", Atomic.builderFor(0.toByte), 782.toByte, Byte.MaxValue, Byte.MinValue)

object AtomicCharSuite extends AtomicNumberSuite[Char, AtomicChar](
  "AtomicChar", Atomic.builderFor(0.toChar), 782.toChar, Char.MaxValue, Char.MinValue)

object AtomicNumberAnySuite extends AtomicNumberSuite[Long, AtomicNumberAny[Long]](
  "AtomicNumberAny", AtomicBuilder.AtomicNumberBuilder[Long], Long.MaxValue, Long.MaxValue, Long.MinValue)
