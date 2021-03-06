package scalax.concurrent.atomic

final class AtomicFloat private[atomic]
  (initialValue: Float) extends AtomicNumber[Float] {

  private[this] var ref = initialValue

  def getAndSet(update: Float): Float = {
    val current = ref
    ref = update
    current
  }

  def compareAndSet(expect: Float, update: Float): Boolean = {
    if (ref == expect) {
      ref = update
      true
    }
    else
      false
  }

  def set(update: Float): Unit = {
    ref = update
  }

  def get: Float = ref

  @inline
  def update(value: Float): Unit = set(value)

  @inline
  def `:=`(value: Float): Unit = set(value)

  @inline
  def lazySet(update: Float): Unit = set(update)

  def transformAndExtract[U](cb: (Float) => (U, Float)): U = {
    val (r, update) = cb(ref)
    ref = update
    r
  }

  def transformAndGet(cb: (Float) => Float): Float = {
    val update = cb(ref)
    ref = update
    update
  }

  def getAndTransform(cb: (Float) => Float): Float = {
    val current = ref
    ref = cb(ref)
    current
  }

  def transform(cb: (Float) => Float): Unit = {
    ref = cb(ref)
  }

  def getAndSubtract(v: Float): Float = {
    val c = ref
    ref = ref - v
    c
  }

  def subtractAndGet(v: Float): Float = {
    ref = ref - v
    ref
  }

  def subtract(v: Float): Unit = {
    ref = ref - v
  }

  def getAndAdd(v: Float): Float = {
    val c = ref
    ref = ref + v
    c
  }

  def getAndIncrement(v: Int = 1): Float = {
    val c = ref
    ref = ref + v
    c
  }

  def addAndGet(v: Float): Float = {
    ref = ref + v
    ref
  }

  def incrementAndGet(v: Int = 1): Float = {
    ref = ref + v
    ref
  }

  def add(v: Float): Unit = {
    ref = ref + v
  }

  def increment(v: Int = 1): Unit = {
    ref = ref + v
  }

  def countDownToZero(v: Float = 1): Float = {
    val current = get
    if (current != 0) {
      val decrement = if (current >= v) v else current
      ref = current - decrement
      decrement
    }
    else
      0
  }

  def decrement(v: Int = 1): Unit = increment(-v)
  def decrementAndGet(v: Int = 1): Float = incrementAndGet(-v)
  def getAndDecrement(v: Int = 1): Float = getAndIncrement(-v)
  def `+=`(v: Float): Unit = addAndGet(v)
  def `-=`(v: Float): Unit = subtractAndGet(v)
}

object AtomicFloat {
  def apply(initialValue: Float): AtomicFloat =
    new AtomicFloat(initialValue)
}