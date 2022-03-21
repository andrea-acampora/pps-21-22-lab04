package u04lab.code

trait Complex:
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers

object Complex:
  def apply(re: Double, im: Double): Complex = ComplexImpl(re, im)
  private case class ComplexImpl(override val re: Double, override val im: Double) extends Complex :
    override def +(c: Complex): Complex = ComplexImpl(re + c.re, im + c.im)
    override def *(c: Complex): Complex = ComplexImpl(re * c.re - im * c.im, re * c.im + im * c.re)

/** Hints:
  *   - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  *   - check that equality and toString do not work
  *   - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  *   - check equality and toString now
  */
