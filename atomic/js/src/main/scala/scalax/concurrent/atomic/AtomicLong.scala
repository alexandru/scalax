package scalax.concurrent.atomic

final class AtomicLong private[atomic]
  (initialValue: Long) extends AtomicNumber[Long] {
  
  private[this] var ref = initialValue

  def getAndSet(update: Long): Long = {
    val current = ref
    ref = update
    current
  }

  def compareAndSet(expect: Long, update: Long): Boolean = {
    if (ref == expect) {
      ref = update
      true
    }
    else
      false
  }

  def set(update: Long): Unit = {
    ref = update
  }

  def get: Long = ref

  @inline
  def update(value: Long): Unit = set(value)

  @inline
  def `:=`(value: Long): Unit = set(value)

  @inline
  def lazySet(update: Long): Unit = set(update)

  def transformAndExtract[U](cb: (Long) => (U, Long)): U = {
    val (r, update) = cb(ref)
    ref = update
    r
  }

  def transformAndGet(cb: (Long) => Long): Long = {
    val update = cb(ref)
    ref = update
    update
  }

  def getAndTransform(cb: (Long) => Long): Long = {
    val current = ref
    ref = cb(ref)
    current
  }

  def transform(cb: (Long) => Long): Unit = {
    ref = cb(ref)
  }

  def getAndSubtract(v: Long): Long = {
    val c = ref
    ref = ref - v
    c
  }

  def subtractAndGet(v: Long): Long = {
    ref = ref - v
    ref
  }

  def subtract(v: Long): Unit = {
    ref = ref - v
  }

  def getAndAdd(v: Long): Long = {
    val c = ref
    ref = ref + v
    c
  }

  def getAndIncrement(v: Int = 1): Long = {
    val c = ref
    ref = ref + v
    c
  }

  def addAndGet(v: Long): Long = {
    ref = ref + v
    ref
  }

  def incrementAndGet(v: Int = 1): Long = {
    ref = ref + v
    ref
  }

  def add(v: Long): Unit = {
    ref = ref + v
  }

  def increment(v: Int = 1): Unit = {
    ref = ref + v
  }

  def countDownToZero(v: Long = 1L): Long = {
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
  def decrementAndGet(v: Int = 1): Long = incrementAndGet(-v)
  def getAndDecrement(v: Int = 1): Long = getAndIncrement(-v)
  def `+=`(v: Long): Unit = addAndGet(v)
  def `-=`(v: Long): Unit = subtractAndGet(v)
}

object AtomicLong {
  def apply(initialValue: Long): AtomicLong =
    new AtomicLong(initialValue)
}
