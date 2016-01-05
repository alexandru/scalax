package scalax.concurrent.atomic

trait AtomicBuilder[T, R <: Atomic[T]] {
  def buildInstance(initialValue: T): R
}

private[atomic] object Implicits {
  trait Level1 {
    implicit def AtomicRefBuilder[T] = new AtomicBuilder[T, AtomicAny[T]] {
      def buildInstance(initialValue: T) =
        AtomicAny(initialValue)
    }
  }

  trait Level2 extends Level1 {
    implicit def AtomicNumberBuilder[T : Numeric] =
      new AtomicBuilder[T, AtomicNumberAny[T]] {
        def buildInstance(initialValue: T) =
          AtomicNumberAny(initialValue)
      }
  }

  trait Level3 extends Level2 {
    implicit val AtomicIntBuilder: AtomicBuilder[Int, AtomicInt] =
      new AtomicBuilder[Int, AtomicInt] {
        def buildInstance(initialValue: Int) =
          AtomicInt(initialValue)
      }

    implicit val AtomicLongBuilder: AtomicBuilder[Long, AtomicLong] =
      new AtomicBuilder[Long, AtomicLong] {
        def buildInstance(initialValue: Long) =
          AtomicLong(initialValue)
      }

    implicit val AtomicBooleanBuilder: AtomicBuilder[Boolean, AtomicBoolean] =
      new AtomicBuilder[Boolean, AtomicBoolean] {
        def buildInstance(initialValue: Boolean) =
          AtomicBoolean(initialValue)
      }

    implicit val AtomicByteBuilder: AtomicBuilder[Byte, AtomicByte] =
      new AtomicBuilder[Byte, AtomicByte] {
        def buildInstance(initialValue: Byte): AtomicByte =
          AtomicByte(initialValue)
      }

    implicit val AtomicCharBuilder: AtomicBuilder[Char, AtomicChar] =
      new AtomicBuilder[Char, AtomicChar] {
        def buildInstance(initialValue: Char): AtomicChar =
          AtomicChar(initialValue)
      }

    implicit val AtomicShortBuilder: AtomicBuilder[Short, AtomicShort] =
      new AtomicBuilder[Short, AtomicShort] {
        def buildInstance(initialValue: Short): AtomicShort =
          AtomicShort(initialValue)
      }

    implicit val AtomicFloatBuilder: AtomicBuilder[Float, AtomicFloat] =
      new AtomicBuilder[Float, AtomicFloat] {
        def buildInstance(initialValue: Float): AtomicFloat =
          AtomicFloat(initialValue)
      }

    implicit val AtomicDoubleBuilder: AtomicBuilder[Double, AtomicDouble] =
      new AtomicBuilder[Double, AtomicDouble] {
        def buildInstance(initialValue: Double): AtomicDouble =
          AtomicDouble(initialValue)
      }
  }
}

object AtomicBuilder extends Implicits.Level3
