object rationals{
  val x = new Rational(1,2)
  val y = new Rational(5,7)
  val z = new Rational(3,2)
  x.num
  x.denom
  x.add(y)
}

class Rational(x: Int, y: Int){
  def num = x
  def denom = y

  def add(that: Rational) = {
    new Rational(num * that.denom + denom * that.num, denom * that.denom)
  }
  def neg = {
    new Rational(-num, denom)
  }
  override def toString = {
    num + "/" + denom
  }

  def sub(that: Rational) = add(that.neg)
}